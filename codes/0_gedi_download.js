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

Map.addLayer(treeCanopyCover,{palette:['darkgreen']},'treeCanopyCover',false);

//2 Plantation and natural forest
var China_nat_forest_raw = LCJ_forest.updateMask(LCJ_forest.eq(2)).updateMask(treeCanopyCover);
var plantation_map_plt_raw = LCJ_forest.updateMask(LCJ_forest.eq(1)).updateMask(treeCanopyCover);
Map.addLayer(plantation_map_plt_raw,{palette:['blue']},'Plantation_forest',false); 
Map.addLayer(China_nat_forest_raw,{palette:['green']},'Natural_forest',false); 


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
                            .map(qualityMask).median();
print(gedi_rh_quality)
var gedi_rh_mean_ic =  gedi_rh_quality//.select(['rh98','digital_elevation_model'])
Map.addLayer(gedi_rh_mean_ic,{},'l2a_property_check',false);

var gedi_rh_mean_ic =  gedi_rh_quality.select(['rh98','digital_elevation_model','delta_time'])

  
Map.addLayer(gedi_rh_mean_ic,
  {bands:['rh98'],palette: 'darkred,red,orange,green,darkgreen',min:1,max:60},
  'rh98',false); 

var gedi_metrics_quality = gedil2b.filter(ee.Filter.calendarRange(4, 9, 'month'))
                                    .map(qualityMask2).mean();

Map.addLayer(gedi_metrics_quality,{},'l2b_property_check',false);
var gedi_metrics_mean_ic = gedi_metrics_quality.select(['cover','pai','fhd_normal','delta_time']);

var gedi_metrics_mean_ic = gedi_metrics_mean_ic.addBands(gedi_rh_quality.select('digital_elevation_model'))

//4 add variables
//4-1 forest age
var forest_age = forest_age.rename('forest_age').unmask(-9999);
Map.addLayer(forest_age,{},'forest_age',false);

//4-2 bioclimate
var bioclimate = ee.Image('projects/ee-modislst/assets/wc21_2_5m_COMPOSITE').unmask(-9999);

var annualMeanTemperature = bioclimate.select('b1');
var visParams = {
  min: -23,
  max: 30,
  palette: ['blue', 'purple', 'cyan', 'green', 'yellow', 'red'],
};
Map.addLayer(annualMeanTemperature, visParams, 'Annual Mean Temperature',false);

//4-3 soil properties
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

print(isric_nitrogen.bandNames(),isric_phh20.bandNames(),isric_clay.bandNames(),isric_soc.bandNames())
var soil_layers = isric_nitrogen.addBands(isric_phh20).addBands(isric_clay).addBands(isric_soc).unmask(-9999)
Map.addLayer(soil_layers,{},"soil_layers",false)
//add bands
var gedi_rh_mean_ic = gedi_rh_mean_ic.addBands(forest_age)
                                     .addBands(bioclimate)
                                     .addBands(soil_layers);
var gedi_metrics_mean_ic = gedi_metrics_mean_ic
                                     // .addBands(forest_age)
                                     // .addBands(bioclimate)
                                     // .addBands(soil_layers);
                                     
print(gedi_rh_mean_ic)
print(gedi_metrics_mean_ic)

//mask to plantation forest and natural forest
var rh_nat_bandnames = gedi_rh_mean_ic.bandNames().map(function(i){
  return ee.String('nat_').cat(i)
})
var rh_plt_bandnames = gedi_rh_mean_ic.bandNames().map(function(i){
  return ee.String('plt_').cat(i)
})

print(rh_nat_bandnames,rh_plt_bandnames)


var gedi_rh_mean_ic = gedi_rh_mean_ic.updateMask(gedi_rh_mean_ic.select(['rh98']))
                                     .updateMask(gedi_metrics_mean_ic.select(['pai']));


var gedi_rh_nat = gedi_rh_mean_ic.rename(rh_nat_bandnames)
                                 .updateMask(China_nat_forest_raw);
                                 
                                 
var gedi_rh_plt = gedi_rh_mean_ic.rename(rh_plt_bandnames)
                                 .updateMask(plantation_map_plt_raw);

Map.addLayer(gedi_rh_nat,{bands:['nat_rh98'],
  palette: 'darkred,red,orange,green,darkgreen',
  min:1,
  max:80},'gedi_rh_nat',false); 
  
Map.addLayer(gedi_rh_plt,{bands:['plt_rh98'],
  palette: 'darkred,red,orange,green,darkgreen',
  min:1,
  max:80},'gedi_rh_plt',false); 

var metric_nat_bandnames = gedi_metrics_mean_ic.bandNames().map(function(i){
  return ee.String('nat_').cat(i)
})
var metric_plt_bandnames = gedi_metrics_mean_ic.bandNames().map(function(i){
  return ee.String('plt_').cat(i)
})
print(metric_nat_bandnames,metric_plt_bandnames)

var gedi_metrics_mean_ic = gedi_metrics_mean_ic.updateMask(gedi_rh_mean_ic.select(['rh98']))
                                               .updateMask(gedi_metrics_mean_ic.select(['pai']));

var gedi_metrics_nat = gedi_metrics_mean_ic.rename(metric_nat_bandnames)
                                            .updateMask(China_nat_forest_raw);
                                            
                                            
var gedi_metrics_plt = gedi_metrics_mean_ic.rename(metric_plt_bandnames)
                                            .updateMask(plantation_map_plt_raw);

Map.addLayer(gedi_metrics_plt,{bands:['plt_cover'],palette:['green']},'plt_cover',false)
Map.addLayer(gedi_metrics_nat,{bands:['nat_cover'],palette:['green']},'nat_cover',false)

// Map.addLayer(gedi_metrics_plt,{bands:['p_ele_b'],palette:['red']},'p_ele_b',false)

// 5 downsample
// 5-1 create patches
var china_grids = China_bd.geometry().coveringGrid('EPSG:3857',25000)
Map.addLayer(china_grids,{color:'orange'},'china_grids',false)
print(china_grids.limit(10))

china_grids = china_grids.randomColumn('rand');
china_grids = china_grids.map(function(f) {
  var rand = ee.Number(f.get('rand'));
  var batch = rand.multiply(10).floor().add(1); // [1,10]
  return f.set('batch', batch);
});

print(china_grids.limit(10))

// 5-2 downsample
var china_grids_result = china_grids.map(function(grid){
  var nat_list = gedi_rh_nat.reduceRegion({
    reducer:ee.Reducer.toList(),
    geometry:grid.geometry(),
    scale:scale,
    tileScale:8,
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
    tileScale:8,
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
    tileScale:8,
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
    tileScale:8,
    maxPixels:1e13
  })
  var grid_result_i = grid.set(plt_list)
  return grid_result_i
})

print(china_grids_result.limit(10))

//print(plt_rh)
//print(china_grids_result.first().propertyNames())
// Export.table.toDrive({
//   collection: china_grids_result,
//   folder:'China_nat_plt_structure_rev1',
//   description: 'china_grids_result_masknan4_25km',
//   fileFormat: 'csv'
// });

for (var i = 1; i <= 10; i++) {
  var batch_i = china_grids_result.filter(ee.Filter.eq('batch', i));

  Export.table.toDrive({
    collection: batch_i,
    folder: 'China_nat_plt_structure_rev2',
    description: 'china_grids_batch_' + i + '_25km',
    fileFormat: 'CSV'
  });
}

// Export.table.toDrive({
//   collection: china_grids_result.limit(120),
//   folder:'China_nat_plt_structure_rev1',
//   description: 'china_grids_result_masknan4_test',
//   fileFormat: 'csv'
// });
