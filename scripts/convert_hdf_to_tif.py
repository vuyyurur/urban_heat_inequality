from osgeo import gdal

hdf_file = "data/modis_temp/MOD11A1.A2020183.h11v04.061.2021012102518.hdf"
output_tif = "data/modis_temp.tif"
dataset = gdal.Open(hdf_file)
subdataset = dataset.GetSubDatasets()[0][0]  # LST_Day_1km band
gdal.Translate(output_tif, subdataset)
print("Converted to data/modis_temp.tif")