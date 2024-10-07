var world_country = ee.FeatureCollection("projects/ee-modislst/assets/logging_spc_dist_datasets/world_countries_241"),
    forest_age = ee.Image("projects/ee-modislst/assets/V_structure/China_Forest_Age"),
    vegregion = ee.FeatureCollection("projects/ee-modislst/assets/china_veg_region_WGS84"),
    LCJ_forest = ee.Image("projects/ee-modislst/assets/V_structure/China_plt_nat_map_LCJ");

//1 China boundary
var China_bd = world_country.filter(ee.Filter.eq("SOC",'CHN')).first();  
//Map.addLayer(China_bd,{color:'grey'},'China_bd',false)
var China_bounds = ee.Feature(China_bd).bounds();
//conservative type

var tree_canopy_cover = ee.ImageCollection('NASA/MEASURES/GFCC/TC/v3')
                  .filter(ee.Filter.date('2015-01-01', '2015-12-31')).select('tree_canopy_cover').mean();
var treeCanopyCover = tree_canopy_cover.updateMask(tree_canopy_cover.gte(10));

Map.addLayer(treeCanopyCover,{palette:['red']},'treeCanopyCover',false);

//2 Plantation and natural forest
var China_nat_forest_raw = LCJ_forest.updateMask(LCJ_forest.eq(2)).updateMask(treeCanopyCover);
var plantation_map_plt_raw = LCJ_forest.updateMask(LCJ_forest.eq(1)).updateMask(treeCanopyCover);
Map.addLayer(plantation_map_plt_raw,{palette:['blue']},'plantation_map_plt_raw',false); 
Map.addLayer(China_nat_forest_raw,{palette:['green']},'China_nat_forest_raw',false); 


//3 GEDI PROCESS

//slope layer
var SRTM = ee.Image('USGS/SRTMGL1_003');
var slope_mask = ee.Terrain.slope(SRTM.select('elevation')).lte(10).selfMask();
Map.addLayer(slope_mask, {min: 0, max: 1}, 'slope_mask',false);

//GEDI
var gedi = ee.ImageCollection('LARSE/GEDI/GEDI02_A_002_MONTHLY').filterBounds(China_bounds.geometry());
var gedil2b = ee.ImageCollection("LARSE/GEDI/GEDI02_B_002_MONTHLY").filterBounds(China_bounds.geometry());

var projection = gedi.first().projection();
var scale = projection.nominalScale();

//data quality
var qualityMask = function(im) {
  return im.updateMask(im.select('quality_flag').eq(1))
      .updateMask(im.select('degrade_flag').eq(0))
      .updateMask(im.select('rh98').gte(5))
      .updateMask(im.select('sensitivity').gte(0.9))
      .updateMask(slope_mask)
      .updateMask(treeCanopyCover)
      
};

var qualityMask2 = function(im) {
  return im.updateMask(im.select('algorithmrun_flag').eq(1))
            .updateMask(im.select('l2b_quality_flag').eq(1))
            .updateMask(im.select('degrade_flag').eq(0))
            .updateMask(im.select('sensitivity').gte(0.9))
            .updateMask(slope_mask)
            .updateMask(treeCanopyCover)
};


var gedi_rh_quality = gedi.filter(ee.Filter.calendarRange(4, 9, 'month'))
                            .map(qualityMask).mean();
var gedi_metrics_quality = gedil2b.filter(ee.Filter.calendarRange(4, 9, 'month'))
                                    .map(qualityMask2).mean();

var gedi_rh_mean_ic =  gedi_rh_quality.select(['rh25','rh50','rh75','rh98','digital_elevation_model'])
                                      .addBands(forest_age).updateMask(gedi_rh_quality.select(['rh98']));

var gedi_rh_nat = gedi_rh_mean_ic.updateMask(China_nat_forest_raw).rename(['n_rh25','n_rh50','n_rh75','n_rh98','n_ele_a','n_age']);
var gedi_rh_plt = gedi_rh_mean_ic.updateMask(plantation_map_plt_raw).rename(['p_rh25','p_rh50','p_rh75','p_rh98','p_ele_a','p_age']);

Map.addLayer(gedi_rh_nat,{bands:['n_rh98'],
  palette: 'darkred,red,orange,green,darkgreen',
  min:1,
  max:20},'gedi_rh_nat',false); 
  
Map.addLayer(gedi_rh_plt,{bands:['p_rh98'],
  palette: 'darkred,red,orange,green,darkgreen',
  min:1,
  max:20},'gedi_rh_plt',false); 
  
Map.addLayer(gedi_rh_mean_ic,
  {bands:['rh98'],palette: 'darkred,red,orange,green,darkgreen',min:1,max:60},
  'rh98',false); 
  
var gedi_metrics_mean_ic = gedi_metrics_quality.select(['cover','pai','fhd_normal']);

var gedi_metrics_mean_ic = gedi_metrics_mean_ic.addBands(gedi_rh_quality.select('digital_elevation_model'))
                                                .updateMask(gedi_rh_quality.select(['rh98']))
                                                .updateMask(gedi_metrics_mean_ic.select(['cover'])) // consistent with L2A
                                                
var gedi_metrics_nat = gedi_metrics_mean_ic.updateMask(China_nat_forest_raw).rename(['n_cover','n_pai','n_fhd_normal','n_ele_b']);
var gedi_metrics_plt = gedi_metrics_mean_ic.updateMask(plantation_map_plt_raw).rename(['p_cover','p_pai','p_fhd_normal','p_ele_b']);

Map.addLayer(gedi_metrics_plt,{bands:['p_cover'],palette:['green']},'p_cover')
Map.addLayer(gedi_metrics_plt,{bands:['p_ele_b'],palette:['red']},'p_ele_b')
//4 country mean [for test only]
var nat_rh = gedi_rh_nat.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:China_bd.geometry(),
    scale:1000,
    maxPixels:1e13
  })
//print(nat_rh)
var plt_rh = gedi_rh_plt.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:China_bd.geometry(),
    scale:1000,
    maxPixels:1e13
  })
//print(plt_rh)
var nat_met = gedi_metrics_nat.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:China_bd.geometry(),
    scale:1000,
    maxPixels:1e13
  })
//print(nat_met)
var plt_met = gedi_metrics_plt.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:China_bd.geometry(),
    scale:1000,
    maxPixels:1e13
  })
//print(plt_met)

var mean_result = ee.Feature(null).set(nat_rh).set(plt_rh).set(nat_met).set(plt_met)
var mean_result = ee.FeatureCollection([mean_result])
Export.table.toDrive({
  collection: mean_result,
  folder:'LCJ4',
  description: 'mean_result_raw_tcc_10',
  fileFormat: 'csv'
});

// 5 downsample
// 5-1 create patches
var china_grids = China_bd.geometry().coveringGrid('EPSG:3857',50000)
Map.addLayer(china_grids,{color:'orange'},'china_grids',false)

// 5-2 downsample
var china_grids_result = china_grids.map(function(grid){
  var nat_list = gedi_rh_nat.reduceRegion({
    reducer:ee.Reducer.toList(),
    geometry:grid.geometry(),
    scale:scale,
    maxPixels:1e13
  })
  var grid_result_i = grid.set(nat_list)
  return grid_result_i
})
var china_grids_result = china_grids_result.map(function(grid){
  var plt_list = gedi_rh_plt.reduceRegion({
    reducer:ee.Reducer.toList(),
    geometry:grid.geometry(),
    scale:scale,
    maxPixels:1e13
  })
  var grid_result_i = grid.set(plt_list)
  return grid_result_i
})
var china_grids_result = china_grids_result.map(function(grid){
  var nat_list = gedi_metrics_nat.reduceRegion({
    reducer:ee.Reducer.toList(),
    geometry:grid.geometry(),
    scale:scale,
    maxPixels:1e13
  })
  var grid_result_i = grid.set(nat_list)
  return grid_result_i
})
var china_grids_result = china_grids_result.map(function(grid){
  var plt_list = gedi_metrics_plt.reduceRegion({
    reducer:ee.Reducer.toList(),
    geometry:grid.geometry(),
    scale:scale,
    maxPixels:1e13
  })
  var grid_result_i = grid.set(plt_list)
  return grid_result_i
})

//print(plt_rh)
//print(china_grids_result.first().propertyNames())
Export.table.toDrive({
  collection: china_grids_result,
  folder:'LCJ4',
  description: 'china_grids_result',
  fileFormat: 'csv'
});

//6 env variables
//6-1 bioclimate
var bioclimate = ee.Image('projects/ee-modislst/assets/wc21_2_5m_COMPOSITE');

var annualMeanTemperature = bioclimate.select('b1');
var visParams = {
  min: -23,
  max: 30,
  palette: ['blue', 'purple', 'cyan', 'green', 'yellow', 'red'],
};
Map.addLayer(annualMeanTemperature, visParams, 'Annual Mean Temperature',false);

//6-2 soilclimate
var SBIO_0_5cm = ee.Image("projects/crowtherlab/soil_bioclim/SBIO_v2_0_5cm")
var SBIO_5_15cm = ee.Image("projects/crowtherlab/soil_bioclim/SBIO_v2_5_15cm")
//print(SBIO_0_5cm.bandNames())
//print(SBIO_5_15cm.bandNames())

//[
//  "SBIO1_Annual_Mean_Temperature",
//  "SBIO2_Mean_Diurnal_Range",
//  "SBIO3_Isothermality",
//  "SBIO4_Temperature_Seasonality",
//  "SBIO5_Max_Temperature_of_Warmest_Month",
//  "SBIO6_Min_Temperature_of_Coldest_Month",
//  "SBIO7_Temperature_Annual_Range",
//  "SBIO8_Mean_Temperature_of_Wettest_Quarter",
//  "SBIO9_Mean_Temperature_of_Driest_Quarter",
//  "SBIO10_Mean_Temperature_of_Warmest_Quarter",
//  "SBIO11_Mean_Temperature_of_Coldest_Quarter"
//]

//6-3 soil properties
var isric_bdod_mean = ee.Image("projects/soilgrids-isric/bdod_mean");
var isric_cec = ee.Image("projects/soilgrids-isric/cec_mean");
var isric_cfvo = ee.Image("projects/soilgrids-isric/cfvo_mean");
var isric_clay = ee.Image("projects/soilgrids-isric/clay_mean");
var isric_sand = ee.Image("projects/soilgrids-isric/sand_mean");
var isric_silt = ee.Image("projects/soilgrids-isric/silt_mean");
var isric_nitrogen = ee.Image("projects/soilgrids-isric/nitrogen_mean");
var isric_phh20 = ee.Image("projects/soilgrids-isric/phh2o_mean");
var isric_soc = ee.Image("projects/soilgrids-isric/soc_mean");
var isric_ocd = ee.Image("projects/soilgrids-isric/ocd_mean");
var isric_ocs = ee.Image("projects/soilgrids-isric/ocs_mean");

print(isric_nitrogen.bandNames(),isric_phh20.bandNames(),isric_ocs.bandNames())
var soil_layers = isric_nitrogen.addBands(isric_phh20).addBands(isric_clay).addBands(isric_soc)

var soil_bandnames = soil_layers.bandNames()
var soil_bandnames_plt = soil_bandnames.map(function(i){
  return ee.String('plt_').cat(i)
})
var soil_bandnames_nat = soil_bandnames.map(function(i){
  return ee.String('nat_').cat(i)
})

var clim_bandnames = bioclimate.bandNames()
var clim_bandnames_plt = clim_bandnames.map(function(i){
  return ee.String('plt_').cat(i)
})
var clim_bandnames_nat = clim_bandnames.map(function(i){
  return ee.String('nat_').cat(i)
})

var soil_layers_plt = soil_layers.updateMask(plantation_map_plt_raw).rename(soil_bandnames_plt)
var soil_layers_nat = soil_layers.updateMask(China_nat_forest_raw).rename(soil_bandnames_nat)
var clim_layers_plt = bioclimate.updateMask(plantation_map_plt_raw).rename(clim_bandnames_plt)
var clim_layers_nat = bioclimate.updateMask(China_nat_forest_raw).rename(clim_bandnames_nat)

print(soil_layers_plt,soil_layers_nat)
var property_list = ee.List(['system:index','p_age','n_age',
                            bioclimate.bandNames(),
                            soil_layers.bandNames(),
                            clim_bandnames_plt,clim_bandnames_nat,
                            soil_bandnames_plt,soil_bandnames_nat]).flatten()
print(property_list)
var grid_results = china_grids.map(function(grid){
  //1
  var grid_bio = bioclimate.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:grid.geometry(),
    scale:1000,
    maxPixels:1e13,
    crs:'EPSG:3857'
  })
  var plt_age = forest_age.updateMask(plantation_map_plt_raw).reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:grid.geometry(),
    scale:30,
    maxPixels:1e13,
    crs:'EPSG:3857'
  })
  var nat_age = forest_age.updateMask(China_nat_forest_raw).reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:grid.geometry(),
    scale:30,
    maxPixels:1e13,
    crs:'EPSG:3857'
  })
  var grid_soil = soil_layers.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:grid.geometry(),
    scale:250,
    maxPixels:1e13,
    crs:'EPSG:3857'
  })
  //2
  var clim_plt = clim_layers_plt.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:grid.geometry(),
    scale:1000,
    maxPixels:1e13,
    crs:'EPSG:3857'
  })
  var clim_nat = clim_layers_nat.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:grid.geometry(),
    scale:1000,
    maxPixels:1e13,
    crs:'EPSG:3857'
  })
  var soil_plt = soil_layers_plt.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:grid.geometry(),
    scale:250,
    maxPixels:1e13,
    crs:'EPSG:3857'
  })
  var soil_nat = soil_layers_nat.reduceRegion({
    reducer:ee.Reducer.mean(),
    geometry:grid.geometry(),
    scale:250,
    maxPixels:1e13,
    crs:'EPSG:3857'
  })

  var grid_result = grid.set(grid_bio).set(grid_soil)
                        .set('p_age',plt_age.get('b1')).set('n_age',nat_age.get('b1'))
                        .set(clim_plt).set(clim_nat)
                        .set(soil_plt).set(soil_nat)
  return grid_result
})
print(grid_results.limit(5))

Map.addLayer(grid_results,{color:'orange'},'grid_results',false)
Export.table.toDrive({
  collection: grid_results,
  folder:'LCJ4',
  description: 'china_grids_envs_natplt_soil2',
  fileFormat: 'csv',
  selectors:property_list.getInfo()
});