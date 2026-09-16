// Para exportar por tiles la TS
var iterationNum = '9';
var griditaNum = 0;
var escalaGrid = 120000;
var utils = require('users/parevalo_bu/gee-ccdc-tools:ccdcUtilities/api');
var palettes = require('users/gena/packages:palettes');

var uiMod = require('users/JonathanVSV/CCDCmod:ui.js');
var utilsMod = require('users/JonathanVSV/CCDCmod:inputs.js');
var classificationMod = require('users/JonathanVSV/CCDCmod:classification.js');
// For Roy et al harmonization, no use in collection 2
// var lcb = require('users/jstnbraaten/modules:ee-lcb.js');

// Negataive avocado
avocado95 = ee.Image.constant(1).updateMask(avocado95.neq(0))
                                .unmask(0)
                                .eq(0);

Map.addLayer(avocado95, {min: 0, max: 1}, 'avocado95');
// Posiblilidades de mejora:
//1. más puntos y con buffer
//2. Sacar ancillary y hacerlo con puro espectral
//3. Dejar solo aguacate con alta probabilidad (>= 0.8) y lo demás asignarle 
// la segunda clase más probable.
var land = JRC.select('max_extent').eq(0);
var forest = GFC.select('treecover2000')
                .gte(50);

  /*JRC.select('transition')
              .lte(2)
              .unmask(0)
              .neq(1);*/
print('land', land);
Map.addLayer(land, {'min': 0, 'max':1 }, 'Land', false);
Map.addLayer(forest, {'min': 0, 'max':1 }, 'forest', true);
Map.addLayer(JRC, {'min': 0, 'max':1 }, 'JRC', false);

Map.addLayer(roiJ, {'min': 0, 'max':1 }, 'roiJ', false);

// 3.
// Checar los quiebres 2023, ver si se pueden detectar cambios con eso. 

// Verificación multitemporal. Ver el artículo de ¿Arévalo? o Quizás pensar en hacerlo 
// como la lógica de 
// Olofsson
// 4.
// Pensar en cómo analizar la ubicación temporal de las clasificaciones en el segmento. 
// Pensar en que un punto en el medio de un segmento puede ser más confiable que uno en 
// el final  del segmento.

var grid = roiJ.geometry().coveringGrid({
      'proj': 'EPSG:6372', // Or a suitable projected CRS for your area
      'scale': escalaGrid
    }).toList(40);

print('areaTotal km2', ee.FeatureCollection(grid)
                     .union(10)
                     .geometry()
                     .area()
                     .divide(1000000));

Map.addLayer(ee.FeatureCollection(grid), {}, 'grid');

//Map.addLayer(image0, {}, 'image0');
// Son 6 cuadritos
print('grid', grid);

var gridita = ee.Feature(grid.get(griditaNum));
Map.addLayer(gridita, {}, 'grid.get(1)', false);

//gridita = gridita.geometry().coveringGrid({
//      'proj': 'EPSG:6372', // Or a suitable projected CRS for your area
//      'scale': 70000 / 2
//    }).toList(20);
//print('gridita', gridita);
//gridita = ee.Feature(gridita.get(griditaSub));
//Map.addLayer(gridita, {}, 'grid.get(1) sub', false);

/*
var verifPts = verif_2021.map(function(feat){
  var geom = ee.Feature(feat).geometry().transform('EPSG:6372', 30);
  return ee.Feature(geom).copyProperties(ee.Feature(feat));
  });
*/
// Define parameters
var studyRegion = roiJ,//gridita.geometry(),
    trainPts = train2026_8,
    folderDrive = 'Avocado_Landtrendr',
    classProperty = 'LC_Class',
    avocadoClass = 7,
    // Start and end date for images; lo recomendado es 2000 como start
    startDate = '2000-01-01',
    endDate = '2026-01-01',
    yearObs = 2021,
    // Recommended size in tutorial: 240 m
    pixelSize = 30,
    // dateFormat : 2 millis, 1: yeardecimal, 0:jDays
    dateFormatNum = 1,
    
    //Breakpoint
    segments = ["S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"],
    minObservations = 6,
    minNumOfYearsScaler = 1,
    breakpointBands = ['GREEN','RED','NIR','SWIR1','SWIR2', 'NBR'],
    tMaskBands = ['GREEN','SWIR2'],
    // Target bands para hacer la temporal segmentation
    // Quitar temp, para evitar muchas zonas enmascaradas
    targetBands = ['BLUE','GREEN','RED','NIR','SWIR1','SWIR2',
                   // no se estaban usando los indices para el break detection
                   // ni la clasificacion.
                   'NBR'
                   //,'NDFI','NDVI','GV','NPV','Shade',
                  //'Soil',
                  //'EVI',
                  //'EVI2', 'BRIGHTNESS', 'GREENNESS', 
                  //'WETNESS'
                  ],
    
    //Classification
    bands = ['BLUE','GREEN',
    'RED','NIR','SWIR1','SWIR2',
    'NBR',
    //'NDFI', //'NDVI',
    //'GV','NPV','Shade', 'Soil'//,
    //'EVI', 'EVI2', 
    //'BRIGHTNESS', 'GREENNESS', 'WETNESS'
                  ],
    inputFeaturesClass = ["INTP", "SLP","PHASE","AMPLITUDE", "RMSE", 
    "PHASE2","AMPLITUDE2", "PHASE3","AMPLITUDE3"
    ],
    // Nueva propuesta
    //["INTP", "SLP", "RMSE", "PHASE","PHASE2","PHASE3",
    //                       "AMPLITUDE","AMPLITUDE2","AMPLITUDE3",
    //                      "COS", "SIN","COS2","SIN2","COS3","SIN3"],
    coefsInputClass = ["INTP", "SLP","COS", "SIN","RMSE","COS2",
                    "SIN2","COS3","SIN3"],
    ancillary = ['ELEVATION', 'DEM_SLOPE', 'ASPECT', 'POPULATION',
                'WATER_OCCURRENCE', 'TREE_COVER',
                'NIGHT_LIGHTS',
                'TEMPERATURE', 'RAINFALL'],
    // Visualization
    classOfInterest = 2,
    classOfInterest2 = 6,
    // Date classes to evaluate change
    // Suggested not very start and end since segments sometimes are not fitted
    // in the extremes
    dateClass1 = '2011-01-01',
    dateClass2 = '2024-01-01',
    dateBreakPoint1 = '2021-01-01',
    dateBreakPoint2 = '2023-01-01',
    // Change vars params
    // Define bands to select to get coefs images.
    SELECT_BANDS = ['RED', 'NIR', 'GREEN'],
    // Define coefficients to select. This list contains all possible segments
    SELECT_COEFS = ["INTP", "SLP", "COS", "SIN", "COS2", "SIN2", "COS3", "SIN3", "RMSE"],
    
    //classMode = 'CLASSIFICATION', // Probability
    // Bands to  get surface reflectance for
    SUB_BANDS = ['RED', 'NIR', 'SWIR1', 'SWIR2'];

// Export.table.toDrive({
//   collection: train2026,
//   folder: folderDrive,
//   description: 'training2026_5',
//   fileFormat: 'SHP'
//   //assetId: params.Classification.trainingPathPredictors
//   });

var visParamsClass = { min: 1, max:7, 
      palette: ['yellow','black', 'gray', 'green', 'orange', 'black', 'pink'
      ]};
var visParamsProbs = {
       min: 0.4, max: 1, 
      palette: ['#440154', '#3b528b', '#21918c', '#5ec962', '#fde725']
    };
var vizParamsTimeSeries = {
  red: 'SWIR1', 
  green: 'NIR', 
  blue: 'RED', 
  redMin: 0, redMax: 0.6, 
  greenMin: 0, greenMax: 0.6, 
  blueMin: 0, blueMax: 0.6,
  tsType: "Time series"
};
// Dictionary definition
// Change detection parameters
var changeDetection = {
  breakpointBands: breakpointBands,
  tmaskBands: tMaskBands,
  minObservations: minObservations,
  chiSquareProbability: 0.9975, // original 0.99
  minNumOfYearsScaler: minNumOfYearsScaler,
  dateFormat: dateFormatNum,
  lambda: 20/10000,
  // We can set as 10000 s first option
  maxIterations: 10000
};

// Classification parameters
var classification = {
  bandNames: bands,
  // Agregadas phase 2, 3, así como amplitude, amplitude 2 y 3
  inputFeatures: inputFeaturesClass,
  coefs: coefsInputClass,
  ancillaryFeatures: ancillary,
  resultFormat: 'SegCollection',
  classProperty: classProperty,
  yearProperty: 'year',
  classifier: ee.Classifier.smileRandomForest,
  classifierParams: {
    numberOfTrees: 150,
    variablesPerSplit: null,
    minLeafPopulation: 1,
    bagFraction: 0.5,
    maxNodes: null,
    seed: 5
  },
  outPath: 'CCDC',
  // Increase number of segments here
  segs: segments,
  };

// Options for landsat image collection search
var options = {
  collection: 2,
  start: startDate,
  end: endDate,
  startDoy: 1,
  endDoy: 366,
  // !! Aquí estaba la cosa de por qué superaba la memory limit
  region: studyRegion,//gridita.geometry(),//studyRegion,
  targetBands: targetBands,
  useMask: true,
  // Si se quiere considerar L9, ver mi versión de ccdc, CCDCmod
  sensors: {l4: true, l5: true, l7: true, l8: true, l9:true}
};

var params = {
  start: startDate,
  end: endDate,
  ChangeDetection: changeDetection,
  Classification: classification,
  StudyRegion: studyRegion
};

//Map.addLayer(roi, {}, 'roi');
//print('trainNew24', trainNew24.limit(10));
Map.addLayer(studyRegion, {}, 'studyRegion', true);
Map.addLayer(trainPts, {'min': 0, 'max':1 }, 'trainPtsNew', false);

/*
// Fix some stuff with training points
var trainPts1 = trainPtsRoiJean;
var trainPts2 = trainNew24;

print('trainPts1', trainPts1);

// Quitar los puntos de agua porque se van a ir con la máscara de agua de JRC
trainPts2 = trainPts2.filter(ee.Filter.neq('reclass','Cuerpos de agua'))
                   // Recortar temporalmente los datos de entrenamiento
                   .filter(ee.Filter.gte('year',1995))
                   .filter(ee.Filter.lte('year',2022));

trainPts1 = trainPts1.filter(ee.Filter.eq('reclass','Asentamientos humanos'))
                   // Recortar temporalmente los datos de entrenamiento
                   .filter(ee.Filter.gte('year',1995))
                   .filter(ee.Filter.lte('year',2022));

var trainPts = trainPts2.merge(trainPts1);
*/

// Esto equivale a "Agri", "Bosque", "Cultivos"
/*
print('clases y LC Class', trainPts.reduceColumns(
  ee.Reducer.first().group({
  groupName: 'reclass'}),
  ['reclass', 'LC_Class']));
print('classes', trainPts.aggregate_array('reclass')
                         .distinct());
print('trainPts LC Class', trainPts.aggregate_array('LC_Class')
                         .distinct());
*/
// Filter by date and a location 
var filteredLandsat = utilsMod.getLandsat2(options);
// Add filteredLandsat as the collection to use for change detection
params.ChangeDetection.collection = filteredLandsat;
print('filteredLandsat', filteredLandsat.size());
// Checar imágenes post 2021; ahora ya salen                           
//var check = filteredLandsat.filterDate('2023-01-01', '2024-01-01');
//print('check', check);
//var temp = filteredLandsat.filterDate('2015-01-01', '2016-01-01')
//                          .aggregate_array('system:time_start')
//                          .map(function(date){
//                            return ee.Date(date).format('Y-M-d');
//                          });
//print('temp',temp);


//print('filteredLandsat',filteredLandsat.limit(10));
//var temp = filteredLandsat.filterDate('1984-01-01','2002-01-01')
//                          .map(function(image){
//                            return image.set('year',
//                              ee.Date(image.get('system:time_start'))
//                                .format('Y'));
//                          })
//                          .aggregate_array('year');
//temp = temp.frequency('2001');

// Frecuencias de número de imágenes en ROI
//  Lo más antiguo medio decente es 1993
//1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001
//0,     6,    25,    5,   0,   7,     22,   6,    19,   100,   91,   98,  110,  106,  98 , 109 , 137 , 87

//----------------------Part 1: Temporal Segmentation-------------------------------------
/*
var ccdcResults = ee.Algorithms.TemporalSegmentation.Ccdc(params.ChangeDetection);
//print('ccdcResults',ccdcResults);
*/
//Map.addLayer(ccdcResults, {bands:'changeProb', min: 0, max: 1}, 'ccdcResults');

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Necesita esta descomentado para correr la parte 2
// Create a metadata dictionary with the parameters and arguments used.
var metadata = changeDetection;
metadata.breakpointBands = metadata.breakpointBands.toString();
metadata.tmaskBands = metadata.tmaskBands.toString();
metadata.startDate = startDate;
metadata.endDate = endDate;
metadata.bands = bands.toString();

//print('metadata', metadata);

//Map.addLayer(ccdcResults.select(0),{}, 'image');

// Esto salio muy pesado, así que a usar la colección pre procesada
// 2000 - 2020
/*
Export.image.toAsset({
  image:ccdcResults.set(metadata),
  description: 'TSCcdc00-26_'+pixelSize+'m_g'+griditaNum+'_120kgrid_'+iterationNum,
  assetId: 'CCDC/TSCcdc00-26_'+pixelSize+'m_g'+griditaNum+'_120kgrid_'+iterationNum, 
  region: gridita.geometry(),
  pyramidingPolicy: {
  ".default": 'sample'
  },
  crs: 'EPSG:6372',
  scale: pixelSize,
  priority: 9999,
  maxPixels: 1e12,
  overwrite: true
});
*/

// Part 2: Classification: removed to only compare change detection vs Lanstrendr

//Map.addLayer(results.select(0).randomVisualizer(), {}, 'Seg1 Classification');
//-------------------------Part 3: Get changes--------------------------------------

params.Classification.changeResults = ee.ImageCollection.fromImages([
                                      image1,image2,image3,image4,
                                      image5, image6])
                                      .mosaic();

var dateParams1 = {inputFormat: 3, inputDate: dateClass1, outputFormat: dateFormatNum};
var formattedDate1 = utils.Dates.convertDate(dateParams1);

var dateParams2 = {inputFormat: 3, inputDate: dateClass2, outputFormat: dateFormatNum};
var formattedDate2 = utils.Dates.convertDate(dateParams2);

// Spectral band names. This list contains all possible bands in this dataset
var BANDS = bands;

// Names of the temporal segments
var SEGS = segments;

// Obtain CCDC results in 'regular' ee.Image format
//var ccdc = TS60m;
var ccdImage = utils.CCDC.buildCcdImage(params.Classification.changeResults, 
                                        SEGS.length, 
                                        BANDS);

// Obtain coefficients
var coefs1 = utils.CCDC.getMultiCoefs(ccdImage, 
                                     formattedDate1, 
                                     SELECT_BANDS, 
                                     SELECT_COEFS, 
                                     true, 
                                     SEGS,
                                     // If not a single segment crosses the indicates date
                                     // use the nearest in time AFTER.
                                     'after');
var coefs2 = utils.CCDC.getMultiCoefs(ccdImage, 
                                     formattedDate2, 
                                     SELECT_BANDS, 
                                     SELECT_COEFS, 
                                     true, 
                                     SEGS,
                                     // If not a single segment crosses the indicates date
                                     // use the nearest in time AFTER.
                                     'after');

print('coefs1', coefs1);                                     

Map.addLayer(coefs1,
    {min: 0.03, max: 0.35, bands: ['NIR_INTP', 'RED_INTP', 'GREEN_INTP']},
    'Compuesto NIR R G interceptos '+ dateClass1, false);
Map.addLayer(coefs2,
    {min: 0.03, max: 0.37,  bands: ['NIR_INTP', 'RED_INTP', 'GREEN_INTP']},
    'Compuesto NIR R G interceptos '+ dateClass2, false);
    
// Obtain synthetic image
var synt1 = utils.CCDC.getMultiSynthetic(ccdImage, 
  formattedDate1, 
  1, 
  BANDS, 
  SEGS);

var synt2 = utils.CCDC.getMultiSynthetic(ccdImage, 
  formattedDate2, 
  1, 
  BANDS, 
  SEGS);

Map.addLayer(synt1,
    {min: 0.011, max: 0.39, bands: ['SWIR1', 'NIR', 'SWIR2']},
    'Compuesto SWIR1 NIR SWIR2 '+ dateClass1, false);
Map.addLayer(synt2,
    {min: 0.011, max: 0.40, bands: ['SWIR1', 'NIR', 'SWIR2']},
    'Compuesto SWIR1 NIR SWIR2 '+ dateClass2, false);

var changeStart = dateClass1;
var changeEnd = dateClass2;
var startParams = {inputFormat: 3, inputDate: changeStart, outputFormat: dateFormatNum};
var endParams = {inputFormat: 3, inputDate: changeEnd, outputFormat: dateFormatNum};
var formattedStart = utils.Dates.convertDate(startParams);
var formattedEnd = utils.Dates.convertDate(endParams);

var filteredChanges = utils.CCDC.filterMag(ccdImage, 
                                           formattedStart,
                                           formattedEnd, 
                                           'NBR', 
                                           SEGS);

print('filteredChanges', filteredChanges);

// test
var magnitudeMask = filteredChanges.select('MAG').lte(0);

// Show three possible bands
Map.addLayer(filteredChanges.updateMask(forest.updateMask(avocado95).unmask(0)), {
  bands: 'numTbreak',
  palette: palettes.colorbrewer.YlOrRd[6],
  min:1, max: 8
}, 'Number of time breaks', true);
Map.addLayer(filteredChanges.updateMask(forest.updateMask(avocado95).unmask(0)), {
  bands: 'MAG',
  palette: palettes.colorbrewer.RdBu[4],
  min:-0.2, max: 0.2
}, 'Magnitude of highest change', true);
Map.addLayer(filteredChanges.updateMask(forest.updateMask(avocado95).unmask(0)), {
  bands: 'tBreak',
  palette: palettes.matplotlib.viridis[7],
  min:1993, max: 2022
}, 'Time of highest magnitude change', true);

var expIm = filteredChanges.select('tBreak')
                           .addBands(filteredChanges.select('MAG').multiply(1000))
                           .addBands(filteredChanges.select('numTbreak'))
                           .toInt16()
                           .updateMask(forest.updateMask(avocado95).unmask(0));

Map.addLayer(expIm, {bands: 'tBreak', min: 2017, max: 2018}, 'time break');

Export.image.toDrive({
  image: expIm,
  description:  'CCDCChanges_'+'00'+'_'+'26'+'_forestAvocadoMask',
  folder: folderDrive,
  region: studyRegion,
  crs: 'EPSG:6372',
  scale: pixelSize
});
