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
// run landtrendr
var ltResults = ltgee.runLT(startYear, endYear, startDay, endDay, roi, changeParams.index, [], runParams, maskThese);

// Want to see time series?
// var annualIm = buildSRcollection(startYear, endYear, startDay, endDay, roi, maskThese);

// Get greatest change map according to changeParams
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