# Create maps of distance from corridors

# Call python
python

# Import modules
import os
import grass.script as grass
from grass.pygrass.modules.shortcuts import general as g
from grass.pygrass.modules.shortcuts import raster as r
from grass.pygrass.modules import Module

# r.in.gdal
r.in_gdal = Module('r.in.gdal')

# Import maps - with zone
folder = r'/home/leecb/Documentos/Academico/artigos/ms_Pena_etal_corredores_OuroPreto/corridors/simulated_corridors/corredores_com_zona_2018_04_d09'
os.chdir(folder)
maps = os.listdir(folder)
for i in maps:
    if i[-8:] == 'RSFI.tif':
        print i
        name = i.replace('.tif', '')
        r.in_gdal(input = i, output = name, overwrite = True)
        
# Import maps - without zone
folder = r'/home/leecb/Documentos/Academico/artigos/ms_Pena_etal_corredores_OuroPreto/corridors/simulated_corridors/corredores_sem_zona'
os.chdir(folder)
maps = os.listdir(folder)
for i in maps:
    if i[-8:] == 'RSFI.tif':
        print i
        name = i.replace('.tif', '')
        r.in_gdal(input = i, output = name, overwrite = True)
 