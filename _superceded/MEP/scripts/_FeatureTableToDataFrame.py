'''
https://gist.github.com/d-wasserman/e9c98be1d0caebc2935afecf0ba239a0
Functions to convert a ArcGIS Table/Feature Class in arcpy to a pandas dataframe. For other options, check the new ArcGIS Python API, but this works across versions.
'''

import arcpy
import pandas as pd

def fn_arcgis_table_to_df(in_fc, input_fields=None, query=""):
    """Function will convert an arcgis table into a pandas dataframe with an object ID index, and the selected
    input fields using an arcpy.da.SearchCursor.
    :param - in_fc - input feature class or table to convert
    :param - input_fields - fields to input to a da search cursor for retrieval
    :param - query - sql query to grab appropriate values
    :returns - pandas.DataFrame"""
    OIDFieldName = arcpy.Describe(in_fc).OIDFieldName
    if input_fields:
        final_fields = [OIDFieldName] + input_fields
    else:
        final_fields = [field.name for field in arcpy.ListFields(in_fc)]
    data = [row for row in arcpy.da.SearchCursor(in_fc,final_fields,where_clause=query)]
    fc_dataframe = pd.DataFrame(data,columns=final_fields)
    fc_dataframe = fc_dataframe.set_index(OIDFieldName,drop=True)
    return fc_dataframe

def fn_arcgis_table_to_np_to_pd_df(in_fc, input_fields, query="", skip_nulls=False, null_values=None):
    """Function will convert an arcgis table into a pandas dataframe with an object ID index, and the selected
    input fields. Uses TableToNumPyArray to get initial data.
    :param - in_fc - input feature class or table to convert
    :param - input_fields - fields to input into a da numpy converter function
    :param - query - sql like query to filter out records returned
    :param - skip_nulls - skip rows with null values
    :param - null_values - values to replace null values with.
    :returns - pandas dataframe"""
    OIDFieldName = arcpy.Describe(in_fc).OIDFieldName
    if input_fields:
        final_fields = [OIDFieldName] + input_fields
    else:
        final_fields = [field.name for field in arcpy.ListFields(in_fc)]
    np_array = arcpy.da.TableToNumPyArray(in_fc, final_fields, query, skip_nulls, null_values)
    object_id_index = np_array[OIDFieldName]
    fc_dataframe = pd.DataFrame(np_array, index=object_id_index, columns=input_fields)
    return fc_dataframe
