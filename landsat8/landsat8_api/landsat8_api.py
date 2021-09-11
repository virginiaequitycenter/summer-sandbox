# load libraries
import json
from logging import log
import requests
from dotenv import dotenv_values
import sys
import os
import time
import re
import geopandas as gpd
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from shapely.geometry import mapping
import rioxarray as rxr
import rasterio as rio
import xarray as xr 
import earthpy as et
import earthpy.plot as ep
from shapely.geometry import Polygon
from datetime import datetime
from tqdm import tqdm 
import pytz
import threading

# USGS/EROS Inventory Service Documentation (Machine-to-Machine API) 
class Landsat8_API_Accessor:
    def __init__(self, username, password):
        # API base URL
        self.SERVICE_URL = "https://m2m.cr.usgs.gov/api/api/json/stable/"
        self.apiKey = self.login(username, password)
        self.threads = []
        self.MAX_THREADS = 5
        self.sema = threading.Semaphore(value=self.MAX_THREADS)

    def login(self, username, password):
        """
        Authenticates user given username and password and returns API key.
        
        Parameters
        ----------
        username : str, required
            USGS account username.
            
        password : str, required
            USGS account password. 
            
        Notes 
        -----
        Go to https://ers.cr.usgs.gov/profile/access to request access 
        to the API and/or make an account.
        
        """
        # login information
        payload = {'username': username, 'password': password}

        # get apiKey 
        apiKey = self.sendRequest(self.SERVICE_URL + "login", payload)
        if apiKey == None:
            print("Login Failed")
        else:
            print("Login Successful")
        
        return apiKey

    def logout(self):
        """
        Invalidates API key. 
        
        Parameters
        ----------
        apiKey : str, required
            Valid API key. Obtain using the login() method defined above.
            
        Notes
        -----
        Make sure to call when you've finished working to ensure that your 
        API key can't be used by an unauthorized user.
        
        """
        if self.sendRequest(self.SERVICE_URL + "logout", None, self.apiKey) == None:
            print("Logged Out\n\n")
        else:
            print("Logout Failed\n\n")

    def sendRequest(self, url, data, apiKey = None):
        """
        Sends HTTPS request to specified API endpoint. Main method for interacting
        with the API.
        
        Parameters
        ----------
        url : str, required
            API endpoint you wish you access. Typical format is SERVICE_URL + endpoint, 
            where endpoint might be something like "login" or "data-search." See https://m2m.cr.usgs.gov/api/docs/reference/
            for all available endpoints.
            
        data : dict, required
            Request payload. Data required changes based on API endpoint. See 
            https://m2m.cr.usgs.gov/api/docs/reference/ for input parameters, sample requests,
            sample and responses for available endpoints.
            
        apiKey : str, optional (default is None)
            Valid API key. Must be speficied for most requests. "login" endpoint doesn't 
            require an API key since you use that endpoint to retrieve a valid API key.
        
        """
        json_data = json.dumps(data)
        
        if apiKey == None:
            response = requests.post(url, json_data)
        else:
            headers = {'X-Auth-Token': apiKey}
            response = requests.post(url, json_data, headers = headers)
            
        try:
            httpStatusCode = response.status_code
            
            if response == None:
                print("No output from service!")
                sys.exit()
                
            output = json.loads(response.text)
            if output['errorCode'] != None:
                print(output['errorCode'], "- ", output['errorMessage'])
                sys.exit()
                
            if httpStatusCode == 404:
                print("404 Not Found")
                sys.exit()
                
            elif httpStatusCode == 401:
                print("401 Unauthorized")
                sys.exit()
                
            elif httpStatusCode == 400:
                print("Error Code", httpStatusCode)
                sys.exit()
                
        except Exception as e:
            response.close()
            print(e)
            sys.exit()
        
        response.close()
        return output['data']

    def __getFilename_fromCd(self, cd):
        """
        Uses content-disposition to infer filename and filetype.
        
        Parameters
        ----------
        cd : str, required
            The Content-Disposition response header from HTTP request 
            to download a file.
            
        Output
        ------
        Inferred filename and type of provided file : str  
        """
        if not cd:
            return None
        fname = re.findall('filename=(.+)', cd)
        if len(fname) == 0:
            return None
        
        return re.sub('\"', '', fname[0]) # remove extra quotes

    def download_file(self, url, downloaded):
        """
        Saves file to local system.
        
        Parameters
        ----------
            url: str, required
                Link to file to be downloaded.
                
        Output
        ------
        Path to downloaded file : str
        """
        self.sema.acquire()
        try:
            res = requests.get(url, stream=True)
            filename = self.__getFilename_fromCd(res.headers.get('content-disposition'))
            print("Downloading {filename}...".format(filename = filename))
            open(filename, 'wb').write(res.content)
            print('Downloaded {filename}.'.format(filename = filename))
            entity_id = "L2ST_" + filename.split('.')[0] + "_TIF"
            downloaded.append(entity_id)
            self.sema.release()
        except Exception as e:
            print("Failed to download from {url}. Will try to re-download.".format(url = url))
            self.sema.release()
            self.runDownload(self.threads, url, downloaded)

    def runDownload(self, threads, url, downloaded):
        thread = threading.Thread(target=self.download_file, args=(url, downloaded,))
        threads.append(thread)
        thread.start()

    def search_scenes(self, bounds, date_range, dataset = "landsat_ot_c2_l2", max_results = 10, cloud_cover = (0, 10), seasons = [6,7,8,9], logging = True):
        """
        Search specified dataset for scenes given spatial and temporal filters.
        
        Parameters
        ----------
        bounds: dict, required
            Dictionary with two entries: 'lowerLeft' and 'upperRight' which contain
            the lower left and upper right lat, lng coordinates of the bounding box covering
            the area of interest.
            
        start_date: str, required
            Format: YYYY-MM-DD
            
        end_date: str, required
            Format: YYYY-MM-DD
            
        dataset: str, optional (default is 'landsat_ot_c2_l2')
            Dataset alias. Use the 'dataset-search' endpoint to discover
            which datasets are available.
            
        cloud_cover_min : int, optional (default is 0)
            Minimum cloud coverage percentage. Scenes with cloud coverage less
            than this value will not be included in the result.
            
        cloud_cover_max: int, optional (default is 10)
            
        """
        apiKey = self.apiKey
        payload = {
            'datasetName': dataset,
            'startingNumber': 1,
            'maxResults': max_results,
            'sceneFilter': {
                'spatialFilter': {
                    'filterType': 'mbr',
                    'lowerLeft': bounds['lowerLeft'],
                    'upperRight': bounds['upperRight']
                },
                'acquisitionFilter': {
                    'start': date_range[0],
                    'end': date_range[1]
                },
                'cloudCoverFilter': {
                    'max': cloud_cover[1],
                    'min': cloud_cover[0],
                    'includeUnknown': False,
                },
                'seasonalFilter': seasons
            },
        }
        
        if logging:
            print("Searching Scenes...")

        entity_ids = []
        scenes = self.sendRequest(self.SERVICE_URL + "scene-search", payload, apiKey)
        for scene in scenes['results']:
            entity_ids.append(scene['entityId'])

        if logging:
            print("Found {num_scenes} Scene(s).".format(num_scenes = scenes['recordsReturned']))
        
        return entity_ids

    def __add_list(self, list_id, entity_ids, dataset = 'landsat_ot_c2_l2', logging = False):
        payload = {
            'listId': list_id,
            'datasetName': dataset,
            'entityIds': entity_ids,
        }
        count = self.sendRequest(self.SERVICE_URL + "scene-list-add", payload, self.apiKey)
        if logging:
            print('Added', count, 'scenes to list', list_id)

    def __remove_list(self, list_id):
        payload = {
            'listId': list_id,
        }
        self.sendRequest(self.SERVICE_URL + "scene-list-remove", payload, self.apiKey)
        
    def __get_product_download_options(self, list_id, dataset = 'landsat_ot_c2_l2', logging = False):
        payload = {
            "listId": list_id,
            "datasetName": dataset,
        }

        products = self.sendRequest(self.SERVICE_URL + "download-options", payload, self.apiKey)
        return products

    def __get_bulk(self, products, downloads):
        for product in products:
            if product['bulkAvailable']:
                downloads.append({"entityId": product["entityId"], "productId": product["id"]})
        return downloads

    def __get_secondaryDownloads(self, products, downloads, file_types = None):
        for product in products:
            if product["secondaryDownloads"] is not None and len(product["secondaryDownloads"]) > 0:
                for secondaryDownload in product["secondaryDownloads"]:
                    if secondaryDownload["bulkAvailable"]:
                        if file_types is None:
                            downloads.append({"entityId": secondaryDownload["entityId"], "productId": secondaryDownload["id"], "scene_entityId": product["entityId"]})
                        else:
                            for file_type in file_types:
                                if secondaryDownload["entityId"][-len(file_type):] == file_type:
                                    downloads.append({"entityId": secondaryDownload["entityId"], "productId": secondaryDownload["id"], "scene_entityId": product["entityId"]})
                                    break # stop iterating through filetypes for this secondaryDownload, filetype has been found
        return downloads

    def __select_products(self, products, download_type, file_types = None):
        downloads = []
        if download_type == "bundle":
            downloads = self.__get_bulk(products, downloads)
        elif download_type == "band":
            downloads = self.__get_secondaryDownloads(products, downloads, file_types=file_types)
        else:
            downloads = self.__get_bulk(products, downloads)
            downloads = self.__get_secondaryDownloads(products, downloads)
        return downloads

    def __request_downloads(self, downloads, label, logging = False):
        payload = {
            'downloads': downloads,
            "label": label,
            'returnAvailable': True
        }

        results = self.sendRequest(self.SERVICE_URL + "download-request", payload, self.apiKey)
        return results

    def __retrieve_downloads(self, label):
        payload = {
            "label": label,
        }
        results = self.sendRequest(self.SERVICE_URL + "download-retrieve", payload, self.apiKey)
        return results

    def __download(self, downloads, label = "test", max_wait_time = 300, logging = True):
        """
        Return entity_ids of downloaded files.

        """
        downloaded = []
        scene_entityId_map = {x['entityId']: x['scene_entityId'] for x in downloads}

        dq_results = self.__request_downloads([{'entityId': x['entityId'], 'productId': x['productId']} for x in downloads], label)
        for result in dq_results['availableDownloads']:
            # to_download = {'entityId': result['entityId'], 'url': result['url']}
            self.runDownload(self.threads, result['url'], downloaded)

        cur_wait_time = 0
        preparingDownloadCount = len(dq_results['preparingDownloads'])
        preparingDownloadIds = []
        if preparingDownloadCount > 0:
            if logging:
                print("Preparing {count} downloads.".format(count = preparingDownloadCount))
            for result in dq_results['preparingDownloads']:
                preparingDownloadIds.append(result['downloadId'])

            dr_results = self.__retrieve_downloads(label)
            if dr_results != False:
                for result in dr_results['available']:
                    if result['downloadId'] in preparingDownloadIds:
                        preparingDownloadIds.remove(result['downloadId'])
                        # to_download = {'entityId': result['entityId'], 'url': result['url']}
                        self.runDownload(self.threads, result['url'], downloaded)

                for result in dr_results['requested']:
                    if result['downloadId'] in preparingDownloadIds:
                        preparingDownloadIds.remove(result['downloadId'])
                        # to_download = {'entityId': result['entityId'], 'url': result['url']}
                        self.runDownload(self.threads, result['url'], downloaded)

            while len(preparingDownloadIds) > 0:
                if logging:
                    print("{num_waiting} downloads are not available yet. Waiting for 30s to retrieve again.".format(num_waiting = len(preparingDownloadIds)))
                
                if cur_wait_time >= max_wait_time:
                    print("{num_waiting} downloads are still unavailable. Exiting download process.".format(num_waiting = len(preparingDownloadIds)))
                    break
                else:
                    time.sleep(30)
                    cur_wait_time += 30
                    dr_results = self.__retrieve_downloads(label)
                    if dr_results != False:
                        for result in dr_results['available']:
                            if result['downloadId'] in preparingDownloadIds:
                                preparingDownloadIds.remove(result['downloadId'])
                                # to_download = {'entityId': result['entityId'], 'url': result['url']}
                                self.runDownload(self.threads, result['url'], downloaded)

        # wait for all threads to finish running
        [t.join() for t in self.threads]

        # reformat downloaded
        downloaded = [{"entityId": x, "scene_entityId": scene_entityId_map[x]} for x in downloaded]

        return downloaded
    
    def get_surface_temperature_data(self, gdf, date_range, cloud_cover = (0, 10), seasons = [6, 7, 8, 9], max_results = 10, logging = False):
        bounds = self.__get_bounds(gdf)
        entity_ids = self.search_scenes(bounds, date_range=date_range, cloud_cover=cloud_cover, seasons=seasons, max_results=max_results, logging=logging)
        list_id = "temp_landsat_ot_c2_l2_list"
        self.__add_list(list_id, entity_ids)
        products = self.__get_product_download_options(list_id)
        downloads = self.__select_products(products, download_type="band", file_types=["ST_B10_TIF"])
        self.__remove_list(list_id)
        downloaded = self.__download(downloads, label = "test_{t}".format(t = str(time.time()).split('.')[0]))
        metadata = self.get_scene_metadata(downloaded)
        return metadata

    def get_acquisitionDates(self, scenes):
        acquisitionDates = {}
        for result in scenes['results']:
            entityId = result['entityId']
            metadata = result['metadata']
            for field in metadata:
                if field['fieldName'] == "Acquisition Date":
                    date = field['value']
                    acquisitionDates[entityId] = date
                    break
        return acquisitionDates

    def __get_bounds(self, gdf):
        """
        Get lower left and upper right (lat, lng) coordinates for all geometries
        in GeoDataFrame.
        
        Parameters
        ----------
        gdf : GeoDataFrame, required
            GeoPandas DataFrame that you want to get a bounding box for.
            
        Output
        ------
        Dictionary containing lower left and upper right (lat, lng) coordinates
        of the bounding box. 
        
        """
        bounds_df = gdf.bounds
        lowerLeft = {'latitude': round(bounds_df['miny'].min(), 6), 'longitude': round(bounds_df['minx'].min(), 6)}
        upperRight = {'latitude': round(bounds_df['maxy'].max(), 6), 'longitude': round(bounds_df['maxx'].max(), 6)}
        return {
            'lowerLeft': lowerLeft,
            'upperRight': upperRight
        }

    def get_scene_metadata(self, downloaded, datasetName = "landsat_ot_c2_l2"):
        scene_metadata = []
        for download in downloaded:
            entity_id = download['scene_entityId']
            new_row = {}

            # call API to get metadata for scene
            res = self.get_individual_scene_metadata(entity_id, datasetName = datasetName)

            # extract metadata of interest
            new_row['file_entityId'] = download['entityId']
            new_row['scene_entityId'] = res['entityId']
            new_row['displayId'] = res['displayId']
            new_row['spatialCoverage'] = res['spatialCoverage']['coordinates'][0]
            new_row['spatialBounds'] = res['spatialBounds']['coordinates'][0]
            fields_of_interest = ['Date Acquired', 'Start Time', 'Stop Time', 'Day/Night Indicator', 'Land Cloud Cover', 'Sun Elevation L0RA', 'Sun Azimuth L0RA', 'Datum']
            for field in res['metadata']:
                name = field['fieldName']
                for interest in fields_of_interest:
                    if name == interest:
                        new_row[interest] = field['value']

            scene_metadata.append(new_row)

        gdf = gpd.GeoDataFrame(scene_metadata, crs='EPSG:4326')
        return gdf

    def get_individual_scene_metadata(self, entity_id, datasetName = "landsat_ot_c2_l2"):
        payload = {
            "entityId": entity_id,
            "datasetName": datasetName,
            "metadataType": "full",
        }

        metadata = self.sendRequest(self.SERVICE_URL + "scene-metadata", payload, self.apiKey)
        return metadata

    
    # def get_scene_metadata(self, scenes):
    #     scene_metadata = []
    #     for result in scenes['results']:
    #         entityId = result['entityId']
    #         acquisitionDate = None
    #         metadata = result['metadata']
    #         for field in metadata:
    #             if field['fieldName'] == "Acquisition Date":
    #                 acquisitionDate = field['value']
    #                 break
    #         cloudCover = result['cloudCover']
    #         publishDate = result['publishDate']
    #         startDate = result['temporalCoverage']['startDate']
    #         endDate = result['temporalCoverage']['endDate']
    #         spatialCoverage = result['spatialCoverage']['coordinates'][0]
    #         spatialBounds = result['spatialBounds']['coordinates'][0]
    #         polygon_geom = Polygon([(x[0], x[1]) for x in spatialCoverage])
    #         new_row = {'entity_id': entityId, 'acquisition_date': acquisitionDate, 'publish_date': publishDate, 'start_date': startDate,
    #                 'end_date': endDate, 'cloud_cover': cloudCover, 'spatial_bounds': spatialBounds, 'spatial_coverage': spatialCoverage,
    #                 'geometry': polygon_geom}        
    #         scene_metadata.append(new_row)
    #     gdf = gpd.GeoDataFrame(scene_metadata, crs="EPSG:4326")
        
    #     # convert date-like cols to date cols
    #     date_cols = ['start_date', 'end_date']
    #     for date_col in date_cols:
    #         gdf[date_col] = gdf[date_col].apply(lambda x: None if x == "Unknown" else x)
    #         gdf[date_col] = gdf[date_col].apply(lambda x: datetime.fromisoformat(x.split('.')[0]) if x is not None else x)
        
    #     # create year col
    #     # gdf['start_year'] = gdf.start_date.apply(lambda x: x.year)
    #     # gdf['end_year'] = gdf.end_date.apply(lambda x: x.year)
        
    #     return gdf
    

    def get_area(self, shp):
        # get area of shp in km^2
        return round(shp.geometry.to_crs("EPSG:3395").map(lambda p: p.area / 10**6).iloc[0], 6)
            
    def intersection_stats(self, shp, scene):
        intersection = gpd.overlay(shp, scene, how='intersection')
        if len(intersection) == 0:
            return {'area': 0, 'shp_percent': 0}
        
        # get area of intersection in km^2
        intersect_area = self.get_area(intersection)
        
        # get area of original shp in km^2
        shp_area = self.get_area(shp)
        
        # compute percentage of intersection of shp
        percentage = (intersect_area / shp_area) * 100
        
        return {'area': intersect_area, 'shp_percent': percentage}
        
    def get_sceneIds(self, scenes):
        """
        Parses scene data to return list of scene ids.
        
        Parameters
        ----------
        scenes : object, required
            Output from search_scenes().
            
        
        Output
        ------
        scene ids : list
        
        """
        sceneIds = []
        for result in scenes['results']:
            sceneIds.append(result['entityId'])
        return sceneIds

    # def download_scenes(self, sceneIds, label, dataset = "landsat_ot_c2_l2", all_bands = True, bands = None):

    def __download_products(self, to_download):
        download_metadata = []
        for req in to_download:
            url = req['url']
            filename = self.download_file(url)
            print("{filename} Downloaded Successfully.".format(filename = filename))
            download_metadata.append({'downloadId': req['downloadId'], 'filename': filename })
        return pd.DataFrame(download_metadata)

    def download_scenes(self, sceneIds, label, dataset = "landsat_ot_c2_l2", all_bands = True, bands = None):
        """
        Downloads scenes.
        
        Parameters
        ----------
        scenes : object, required
            Scenes you wish to download. Returned from search_scenes().
            
        label : str, required
            Label for your download request.
            
        dataset : str, optional (default is 'landsat_ot_c2_l2')
            Must be the dataset the scenes are from. 
            
        Output
        ------
        Paths to downloaded files : list
        """

        if not all_bands and bands is None:
            raise ValueError("bands can't be None if all_bands is False")

        apiKey = self.apiKey

        # download options
        payload = {
            'datasetName': dataset,
            'entityIds': sceneIds,
        }
        
        downloadOptions = self.sendRequest(self.SERVICE_URL + "download-options", payload, apiKey)
        
        # aggregate list of available products
        downloads = []
        seen = {}
        for option in downloadOptions:
            # make sure the product is available for this scene
            if all_bands:
                if option['available'] == True:
                        downloads.append({'entityId': option['entityId'], 'productId': option['id']})
            else:
                for product in option['secondaryDownloads']:
                    for band in bands:
                        band += '.TIF'
                        if product['available'] == True and product['displayId'][-len(band):] == band and product['id'] not in seen:
                            downloads.append({'entityId': product['entityId'], 'productId': product['id']})
                            seen[product['id']] = True
                
        if downloads:
            requestedDownloadsCount = len(downloads)
            print("Number of Requested Downloads: {requestedDownloadsCount}".format(requestedDownloadsCount = requestedDownloadsCount))
            print("Downloading Now...")
            payload = {
                'downloads': downloads,
                'label': label
            }
            requestResults = self.sendRequest(self.SERVICE_URL + "download-request", payload, apiKey)
            
            
            if requestResults['preparingDownloads'] != None and len(requestResults['preparingDownloads']) > 0:
                payload = {'label': label}
                downloadUrls = self.sendRequest(self.SERVICE_URL + "download-retrieve", payload, apiKey)
                available = []
                for download in downloadUrls['available']:
                    available.append({'downloadId': download['downloadId'], 'url': download['url']})
                    
                while len(available) < requestedDownloadsCount:
                    preparingDownloads = requestedDownloadsCount - len(available)
                    print('\n', preparingDownloads, "download(s) are not yet available. Waiting for 30 seconds.\n")
                    time.sleep(30)
                    print("Trying to retrieve data.\n")
                    downloadUrls = self.sendRequest(self.SERVICE_URL + "download-retrieve", payload, apiKey)
                    for download in downloadUrls['available']:
                        if download['downloadId'] not in available:
                            available.append({'downloadId': download['downloadId'], 'url': download['url']})

                download_metadata = self.__download_products(available)
                return download_metadata
            else:
                # get all available downloads
                # search requested downloads to get metadata
                payload = {
                    'label': label
                }
                download_search = self.sendRequest(self.SERVICE_URL + "download-search", payload, apiKey)
                download_metadata = {}
                for search_result in download_search:
                    entityId = search_result['entityId']
                    displayId = search_result['displayId']
                    downloadId = search_result['downloadId']
                    download_metadata[downloadId] = {'entityId': entityId, 'displayId': displayId}
                    
                available = []
                for download in requestResults['availableDownloads']:
                    available.append({'downloadId': download['downloadId'], 'url': download['url']})
                    # url = download['url']
                    # downloadId = download['downloadId']
                    # filename = self.download_file(url)
                    # print("{filename} Downloaded Successfully.".format(filename = filename))
                    # filedata = {
                     #    'entityId': download_metadata[downloadId]['entityId'],
                      #   'displayId': download_metadata[downloadId]['displayId'],
                       #  'filename': filename
                     # }
                     # files.append(filedata)
                download_metadata = self.__download_products(available)
                return download_metadata
            
        else:
            print("No available products.")