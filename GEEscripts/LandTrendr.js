var roiJ = ee.FeatureCollection("users/JonathanVSV/CCDC/roiAguacate"),
    l5 = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2"),
    l7 = ee.ImageCollection("LANDSAT/LE07/C02/T1_L2"),
    l8 = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2"),
    l9 = ee.ImageCollection("LANDSAT/LC09/C02/T1_L2"),
    GLCF = ee.ImageCollection("GLCF/GLS_WATER"),
    JRC = ee.Image("JRC/GSW1_4/GlobalSurfaceWater");

// load the LandTrendr.js module
var ltgee = require('users/JonathanVSV/Defor-DegradDetect:Landtrendr_mod'); 
// For Roy et al 2016 harmonization
var lcb = require('users/jstnbraaten/modules:ee-lcb.js');
//var ltgee = require('users/emaprlab/public:Modules/LandTrendr-braaten-test.js'); 
// previous
var tester = avocado95.unmask(0)
                     .eq(0);

// Negataive avocado
avocado95 = ee.Image.constant(1).updateMask(avocado95.neq(0))
                                .unmask(0)
                                .eq(0);
                     
// avocado95 = ee.Image.constant(1).updateMask(avocado95);

Map.addLayer(tester, {min: 0, max: 1}, 'tester');
Map.addLayer(avocado95, {min: 0, max: 1}, 'avocado95');

// Consider using delta = gain for avocado orchard monitoring
// Changed to NBR to facilitate comparison with CCDC (already implemented).
//spectral band or index that is to be segmented is oriented so that vegetation loss
// is represented by a positive delta

var roi = geometry3,
    // Solo usar meses de lluvias
    startMonth = 6,
    endMonth = 12,
    dateStart = '2000-01-01',
    dateEnd = '2026-01-01',
    startYear = 2000,
    endYear = 2026,
    yearBand = '2025',
    folder = 'Avocado_Landtrendr';

// Reduce the LandTrendr result by the aoi. Use ee.Reducer.first() to select
// the pixel that intersects the point.
// Se Time trend in console plot.
// Avocado plantation after 2015
var aoii = ee.FeatureCollection([
  ee.Feature(ee.Geometry.Point(-101.7896615643898, 19.339343878910718), {'id': 0}) //35.219060, 24.034525. (24.031278, 35.217972)
]);

var imageVisParam = {"opacity":1,
                     "bands":["B6","B5","B4"],
                     "min":0.10412373597710393,
                     "max":0.3148032638098514,
                     "gamma":1}; 

// set visualization dictionaries

var palette = ['#FFFFFF',
'#FFCCCC',
'#FF6666',
'#FF0000']; //for visualising values less than 0.1 add '#ffffff' => 4 classes

// NBR values are scaled by 1000
var magVizParms = {
  min: 200, 
  max: 500, //for 4 classes put 0.3
  palette: palette
};

var palette2 = ['#ffffd4','#fed98e','#fe9929','#cc4c02'];
var yodVizParms = {
  min: startYear,
  max: endYear,
  palette: palette2
};

// Forest mask
var forest = GFC.select('treecover2000')
                .gte(50);

Map.addLayer(forest, {min: 0, max: 1}, 'forest');
////----------------------------------------------------------------------

// Params for running LandTrendR
var runParams = {
  maxSegments:            8, // Changed to 8 as CCDC // change to 6
  spikeThreshold:         0.95, // original 0.9
  vertexCountOvershoot:   1, // original 3
  preventOneYearRecovery: true,
  recoveryThreshold:      0.25, //original 0.25
  pvalThreshold:          0.025,// original 0.05
  bestModelProportion:    0.75, // original 0.75,
  minObservationsNeeded:  6 // 6 years as minimum to fit trend
};

// See definitions
// https://emapr.github.io/LT-GEE/api.html#getchangemap
var changeParams = 
  {
   index:  'NBR', // NBR as CCDC
    delta:  'loss',
    sort:   'greatest',
    year:   {checked:true, start:startYear, end:endYear},
    // Increase magnitude: reduce changes
    mag:    {checked:true,  value:100,  operator: '>', dsnr:false}, //for maps=0.1, for validation purposes = 0
    // Increase duration: increase changes
    dur:    {checked:true,  value: 32,    operator: '<'}, // avoid filter by duration
    // Increase value: decrease changes
    preval: {checked:false,  value: 200,  operator: '>'}, // avoid preval filter to see all
    // Increase value: decrease changes
    mmu:    {checked:false,  value:1} // avoid minimum mapping unit filter, as CCDC
  };


//-------------------------------------------------------------------------------
// All this is done by the function ltgee.runLT
// Possibility to simplify script by running this func directly.

 print('l7', l7.limit(10));
 
// Trying to harmonize according to Roy et al., 2016; modified from 'users/jstnbraaten/modules:ee-lcb.js'
// There seems to be no need to harmonize this; harmonization was suggested for Collection 1
// var harmonizeLine = {
//   oliTmTMA: {
//     itcps: ee.Image.constant([-0.0095, -0.0016, -0.0022, -0.0021, -0.0030, 0.0029]).multiply(10000),
//     slopes: ee.Image.constant([0.9785, 0.9542, 0.9825, 1.0073, 1.0171, 0.9949])
//   }
// };

// function tm2oli(tm) {
//   return tm.select(['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7'])
//     .multiply(harmonizeLine.oliTmTMA.slopes)
//     .add(harmonizeLine.oliTmTMA.itcps);
// }

// Function to filter landsat 8 images
var filterImCol = function (imageCol, sensor){
  
  // Make automated band selection
  var bandas = ee.Algorithms.If(sensor == 8,
    ['SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'],
    ['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7']);
  
  // Cloud masking function
  var maskCloud57 = function(image){
    
    var qa = image.select('QA_PIXEL');
    
    var dilCloud = 1 << 1;
    var cloud = 1 << 3;
    var cloudShadow = 1 << 4;
  
    // bitwiseAnd allows you to do operations based on bit values
    var clearMask = qa.bitwiseAnd(cloud).eq(0)
                      .and(qa.bitwiseAnd(cloudShadow).eq(0))
                      .and(qa.bitwiseAnd(dilCloud).eq(0));
                    
    return image.updateMask(clearMask);
  };
  
  var maskCloud8 = function(image){
    
    var qa = image.select('QA_PIXEL');
    
    var dilCloud = 1 << 1;
    var cirrus = 1 << 2;
    var cloud = 1 << 3;
    var cloudShadow = 1 << 4;
  
    // bitwiseAnd allows you to do operations based on bit values
    var clearMask = qa.bitwiseAnd(cloud).eq(0)
                      .and(qa.bitwiseAnd(cirrus).eq(0))
                      .and(qa.bitwiseAnd(cloudShadow).eq(0))
                      .and(qa.bitwiseAnd(dilCloud).eq(0));
                    
    return image.updateMask(clearMask);
  };
  
  var temp = ee.ImageCollection(imageCol)
   .filterDate(dateStart,dateEnd)
   .filter(ee.Filter.calendarRange(startMonth, endMonth, 'month'))
   // Spatial filter
   .filterBounds(roi)
   // Filter by metadata property
   // .filter(ee.Filter.lte('CLOUD_COVER_LAND',50));
   
  
  temp = ee.Algorithms.If(sensor == 8,
    temp.map(maskCloud8),
    temp.map(maskCloud57)
        // .map(tm2oli)
        // Difference with and without harmonization is minimal, so probably everything is fine
        );
    
  // Temporal filter
  temp = ee.ImageCollection(temp)
     // Seleccionar bandas
     .select(bandas)
     // Rename bands
     .map(function(image){
       return image.rename(['B','G','R','NIR','SWIR1','SWIR2']);
     })
     .map(function applyScaleFactors(image) {
        var opticalBands = image.select(['B','G','R','NIR','SWIR1','SWIR2'])
                                .multiply(0.0000275)
                                .add(-0.2);
        
        return image.addBands(opticalBands, null, true);
      })
     .map(function(image){
     var NBR = image.normalizedDifference(['NIR', 'SWIR2'])
      .rename('NBR')
      // Invert as SWIR band so loss are characterized by positive change
      .multiply(-1)
      .multiply(1000)
      .round()
      .toInt();
     return image.addBands(NBR);
   });
  //temp = ee.ImageCollection(temp).copyProperties(ee.ImageCollection(imageCol), 'system:time_start');
  // Copy capture date property
  return temp;
};

// Use function
var l5c = filterImCol(l5, 5);
var l7c = filterImCol(l7, 7);
var l8c = filterImCol(l8, 8);
var l9c = filterImCol(l9, 8);

print('l8c', l8c);
print('l7c', l7c);
print('l5c', l5c);

// Map.addLayer(l5c.first(), {bands:['R','G','B'], min:0.02, max:0.2}, 'l5 test');
// Map.addLayer(l8c.first(), {bands:['R','G','B'], min:0.02, max:0.2}, 'l8 test')

print('l7c', l7c);
print('l5c', l5c);

// Here we already have the collection of landsat 7 and 8 together
var all = l5c.merge(l7c)
             .merge(l8c)
             .merge(l9c);

// Limit 10
print('all', all.limit(10));

// Year to loop over
// 1 year less than final
var años = ee.List.sequence(startYear, endYear-1, 1);
print('años', años);

// Year loop
var resul = años.map(function(year){
  var start = ee.Date.fromYMD(ee.Number(year), 1, 1);
  var end = start.advance(1, 'year');
  //these two lines above used to be (below) creating median composite for the whole year, 
  // now it does the same but only for summer months, because Landtrednr for VI-s used to do the same
  //var start = ee.Date.fromYMD(ee.Number(year), 1, 1);
  //var end = start.advance(1, 'year');
  
  var filt = all.filterDate(start, end)
                .median()
                .clip(roi);
  
  /*
  var bareMean = filt.reduceRegion(ee.Reducer.mean(), bare, 30).values();
  var vegMean = filt.reduceRegion(ee.Reducer.mean(), veg, 30).values();
  
  var unmixedImage = ee.Image(filt).unmix({
                        endmembers: [bareMean, vegMean],
                        sumToOne: true,
                        nonNegative: true
                      }).rename(['bareMean', 'vegMean'])
                      .multiply(-1);
  */
  return filt//.addBands(unmixedImage)
          .set('system:time_start', ee.Number(start.millis()));
});

// No hay imagen 2001, pegar la del 2000 para evitar que las fechas se recorran un año para atrás
// parecía que detectaba el disturbio antes de que ocurriera.

resul = resul.set(1, ee.Image(resul.get(0))
                                   .set('system:time_start', ee.Number(ee.Date.fromYMD(ee.Number(2001), 1, 1).millis())));

var annualMosaicCollection = ee.ImageCollection.fromImages(resul);
print('annualMosaicCollection', annualMosaicCollection);

// Sequence generator function (commonly referred to as "range", e.g. Clojure, PHP etc)
// var añosUser = años.map(function(x){
//   return ee.String(ee.Number(x).int());
// }).evaluate();
var añosUser = [
  /*'1993',
  '1994',
  '1995',
  '1996',
  '1997',
  '1998',
  '1999',*/
  '2000',
  '2001',
  '2002',
  '2003',
  '2004',
  '2005',
  '2006',
  '2007',
  '2008',
  '2009',
  '2010',
  '2011',
  '2012',
  '2013',
  '2014',
  '2015',
  '2016',
  '2017',
  '2018',
  '2019',
  '2020',
  '2021',
  '2022',
  '2023',
  '2024',
  '2025'
];
var bandByYear = annualMosaicCollection.select('NBR');
var bandByYear = bandByYear.toBands()
                           .rename(añosUser)
                           .round()
                           .toInt16();
                           
print('bandByYear',bandByYear);
// Scaled to 1000
//Map.addLayer(bandByYear.multiply, {bands: yearBand, min:-700, max: -87}, 'NBR');
Map.addLayer(bandByYear.multiply(-1), {bands: yearBand, min:-121, max: 734}, 'bandByYear normal', false);

//select study period to download 
//(fitted data observations = total number of observations - 1)
//var NDVI_forecast = NDVI_forecast.select(ee.List.sequence(173,273));
var im_geo = bandByYear.geometry();
print(im_geo);

// Export the image, specifying the CRS, transform, and region.
Export.image.toDrive({
  image: bandByYear.multiply(-1).toInt16(),
  description: 'NBRByYear',
  folder: folder,
  region: roi,
  scale: 30,
  crs: 'EPSG:6372'
});

// Function taken from the original repo
/*
var standardize = function(collection){
  var mean = collection.reduce(ee.Reducer.mean());
  var stdDev = collection.reduce(ee.Reducer.stdDev());
  
  var meanAdj = collection.map(function(img){
    return img.subtract(mean).set('system:time_start', ee.Number(img.get('system:time_start')));
  });
  
  return meanAdj.map(function(img){
    return img.divide(stdDev).multiply(1000).set('system:time_start', ee.Number(img.get('system:time_start')));
  });
};

var annualMosaicCollection = standardize(annualMosaicCollection);
Map.addLayer(annualMosaicCollection.select('vegMean').toBands()
                              .rename(añosUser), 
                              {bands: ['2022'], min: -5000, max: 3500}, 'bandByYear inverse');
*/                              

// Map.centerObject(geometry, 11);
//Map.addLayer(annualMosaicCollection.select(['vegMean']).filter(ee.Filter.eq('system:time_start', ee.Date.fromYMD(1995, 6, 1).millis())), {min: 0,max:1}, 'annualMosaicCollection 2015');
//Map.addLayer(annualMosaicCollection.select(['vegMean']).filter(ee.Filter.eq('system:time_start', ee.Date.fromYMD(2023, 6, 1).millis())), {min: 0, max: .6, palette: palettes.gv}, 'annualMosaicCollection 2023');
print('annualMosaicCollection', annualMosaicCollection);

runParams.timeSeries = annualMosaicCollection.select('NBR');

// Run LandTrendr on Your Image Collection
var ltResults = ee.Algorithms.TemporalSegmentation.LandTrendr(runParams);
print('ltResults',ltResults);
//Map.addLayer(lt, {}, 'lt');

var resul = ltgee.getChangeMap(ltResults, changeParams)
                 .toFloat();

print('change map',resul);

Map.addLayer(resul.select(['mag']).updateMask(forest.updateMask(avocado95).unmask(0)), magVizParms, 'magnitude SU Landtrendr');
Map.addLayer(resul.select(['yod']).updateMask(forest.updateMask(avocado95).unmask(0)), yodVizParms, 'yod SU Landtrendr', false);

print('resul', resul);

Map.addLayer(forest.updateMask(avocado95).unmask(0), {}, 'masker');

Export.image.toDrive({
  image: resul.updateMask(forest.updateMask(avocado95).unmask(0)),
  description: 'LandtrendrGreatChange_Avocado'+'00-26_nocloudFilter_ForestAvocadoMask'+'_'+startMonth+'_'+endMonth,
  folder: folder,
  region: roi,
  scale: 30,
  crs: 'EPSG:6372'
});

var exporter = function(imagen, nombre){
    Export.image.toDrive({
      // Export as integers
      image: imagen.round().toInt16(),
      description: nombre,
      folder: folder,
      region:roi,
      scale: 30,
      maxPixels: 1e12,
      crs: 'EPSG:6372'
    });
};

// Tutorial code + some mods to export as multiband image (pad and flatten)
// 
var lt = ltResults.select('LandTrendr');
var vertexMask = lt.arraySlice(0, 3, 4); // slice out the 'Is Vertex' row - yes(1)/no(0)
var vertices = lt.arrayMask(vertexMask); // use the 'Is Vertex' row as a mask for all rows

var left = vertices.arraySlice(1, 0, -1);    // slice out the vertices as the start of segments
var right = vertices.arraySlice(1, 1, null); // slice out the vertices as the end of segments
var startYear = left.arraySlice(0, 0, 1);    // get year dimension of LT data from the segment start vertices
var startVal = left.arraySlice(0, 2, 3);     // get spectral index dimension of LT data from the segment start vertices
var endYear = right.arraySlice(0, 0, 1);     // get year dimension of LT data from the segment end vertices 
var endVal = right.arraySlice(0, 2, 3);      // get spectral index dimension of LT data from the segment end vertices

var dur = endYear.subtract(startYear);       // subtract the segment start year from the segment end year to calculate the duration of segments 
var mag = endVal.subtract(startVal);         // substract the segment start index value from the segment end index value to calculate the delta of segments
var rate = mag.divide(dur);                  // calculate the rate of spectral change
                     
var segInfo = ee.Image.cat([startYear.add(1), endYear, startVal, endVal, mag, dur, rate])
                      .toArray(0)
                      .mask(vertexMask.mask())
                      // Pad to max extent of segments
                      .arrayPad({lengths: [0, runParams.maxSegments],
                          pad: 0
                      })
                      .arrayFlatten([['startYear', 'endYear', 'startVal', 'endVal',
                        'mag', 'dur', 'rate'], ['0','1','2','3','4','5','6','7']]);
print('segInfo', segInfo);                      
Map.addLayer(segInfo, {}, 'segInfo', false);

exporter(segInfo, 'Lantrendr_fullSegments');

// Exportar máscara de bosques
var bosque = ee.Image.constant(1).updateMask(forest.updateMask(avocado95).unmask(0));
exporter(bosque, 'BosqueMask_geom3');