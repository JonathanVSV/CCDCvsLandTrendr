// Para exportar por tiles la TS
//var griditaNum = 4;
//var escalaGrid = 60000;
var utils = require('users/parevalo_bu/gee-ccdc-tools:ccdcUtilities/api');
var palettes = require('users/gena/packages:palettes');

var uiMod = require('users/JonathanVSV/CCDCmod:ui.js');
var utilsMod = require('users/JonathanVSV/CCDCmod:inputs.js');
var classificationMod = require('users/JonathanVSV/CCDCmod:classification.js');

// Posiblilidades de mejora:
//1. más puntos y con buffer
//2. Sacar ancillary y hacerlo con puro espectral
//3. Dejar solo aguacate con alta probabilidad (>= 0.8) y lo demás asignarle 
// la segunda clase más probable.
var land = JRC.select('max_extent').eq(0);

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
    // for exporting verifdata
    idpt = 1,
    
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
  chiSquareProbability: 0.99,
  minNumOfYearsScaler: 1.33,
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
  outPath: 'PSV',
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
  region: studyRegion,
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

print('clases y LC Class', trainPts.reduceColumns(
  ee.Reducer.first().group({
  groupName: 'reclass'}),
  ['reclass', 'LC_Class']));
print('classes', trainPts.aggregate_array('reclass')
                         .distinct());
print('trainPts LC Class', trainPts.aggregate_array('LC_Class')
                         .distinct());

// Filter by date and a location 
var filteredLandsat = utilsMod.getLandsat2(options);
// Add filteredLandsat as the collection to use for change detection
params.ChangeDetection.collection = filteredLandsat;
print('filteredLandsat', filteredLandsat.limit(10));

var point = ee.Feature(verifLandtCCDC.filter(ee.Filter.eq('name', idpt)).first());

// 7. Create the NDVI time series chart at the point
var chart = ui.Chart.image.series({
  imageCollection: filteredLandsat.select('NBR'),
  region: point,
  reducer: ee.Reducer.mean(),
  scale: 30
}).setOptions({
  title: 'NBR Time Series at Point',
  vAxis: {title: 'NBR', minValue: 0, maxValue: 1},
  hAxis: {title: 'Date'},
  lineWidth: 1,
  pointSize: 3
});

// 8. Print chart to console
print(chart);
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
	description: 'CCDC/TSFranjaAguaCcdc93-24_'+pixelSize+'m_g'+griditaNum,
	region: studyRegion,
	pyramidingPolicy: {
	".default": 'sample'
	},
	crs: 'EPSG:6372',
	scale: pixelSize
});
*/

//---------------------------Part 2: Train RF based on temporal segments-------------------------

// Define path to change detection results
// Esto tiene que estar sin comentar para que jale bien la parte de la clasificación
// Si no sale algo así como cant find select...
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// Esta línea debe estar descomentada para que jale
var TS = ee.ImageCollection.fromImages([image1, image2, image3, image4, image5, image6]);

// juntar en un mosaico las imágenes TS
params.Classification.changeResults = TS.mosaic();

print('params.Classification.changeResults', params.Classification.changeResults);

// Opción 2
// Hay flattenear esto para que se pueda leer por sampleRegions
var imTrans = ee.Image(params.Classification.changeResults).select(['tBreak','tEnd','tStart','changeProb','NBR_.*']);
var fitted = imTrans.reduceRegions({
  collection: verifLandtCCDC,
  reducer: ee.Reducer.firstNonNull(),
  //properties: ['id', 'name', 'Model', 'Proceso', 'Type'],
  scale: 30,
  crs:'EPSG:6372'
});

print('fitted', fitted.limit(10));

// prueba
/*
var propName = 'tBreak';

var resul = ee.List(['tBreak', 'tStart', 'tEnd']).map(function(propName){
  var valueList = ee.Array(ee.Feature(fitted.first()).get(propName)).toList();
  var listSize = valueList.size();
  var indices = ee.List.sequence(0, listSize.subtract(1));
  //print('prueba', valueList);
  //print('tamanio', listSize);
  
  var propDict = indices.map(function(index) {
          index = ee.Number(index);
          var key = ee.String(propName).cat('_').cat(index.format('%d'));
          var value = ee.Number(valueList.get(index));
          return ee.List([key, value]);
        }).flatten();
  return(ee.List(propDict));
  //print('propDict', propDict);
});

var newProps = ee.List(resul).flatten();
print('resul', newProps);

var resul2 = ee.Feature(fitted.first()).set(newProps);
print('resul2', resul2);
*/

// El bueno


var flattenAllLists = function(feature) {
  // List all properties that contain lists (adjust these names)
  // Estos son arrays
  var listProperties = ee.List(['tBreak','tEnd','tStart','changeProb', 'NBR_magnitude', 'NBR_rmse']); // your actual property names
  
  // var allNewProps = ee.Dictionary({});
  
  var newProps = listProperties.map(function(propName) {
    var valueList = ee.Array(ee.Feature(feature).get(ee.String(propName)));
    valueList = valueList.toList();
    var listSize = valueList.size();
    var indices = ee.List.sequence(0, listSize.subtract(1));
    
    var propDict = indices.map(function(index) {
        var indice = ee.Number(index);
        var key = ee.String(propName).cat('_').cat(indice.format('%d'));
        var value = ee.Number(valueList.get(indice));
        return ee.List([key, value]);
      }).flatten();
    
    return(ee.List(propDict));
  }).flatten();
  
  // var allNewProps = ee.Dictionary({});
    var valueList = ee.Array(ee.Feature(feature).get(ee.String('NBR_coefs'))).toList()
    var listSize = valueList.size();
    var indices = ee.List.sequence(0, listSize.subtract(1));
    //Ciclo lista nivel 1
    var propDictCoefs = indices.map(function(index) {
        var indice = ee.Number(index);
        var values2 = ee.List(valueList.get(indice))
        var listSize2 = values2.size();
        var indices2 = ee.List.sequence(0, listSize2.subtract(1));
        
        //Ciclo lista nivel 2
        var propDict2 = indices2.map(function(index2) {
          var indice2 = ee.Number(index2)
          var key = ee.String('NBR_coefs').cat('_').cat(indice.format('%d_')).cat(indice2.format('%d'));
          var valueList2 = ee.List(values2).getNumber(indice2);
          return ee.List([key, valueList2]);
      }).flatten();
      return(ee.List(propDict2).flatten())
    }).flatten();
    
  // Aparte hacer el de 'NBR_coefs' porque ese si es list
  
  return ee.Feature(ee.Feature(feature).geometry(), null)
                      .set(newProps)
                      .set(propDictCoefs)
                      .set('name', ee.Feature(feature).get('name'));
};

var flattenedResults = fitted.map(flattenAllLists);

print('flattenedResults', flattenedResults);

// Create list of properties to be exported
var seqsCoefs = ee.List.sequence(0,8,1);
var seqsTerms = ee.List.sequence(0,9,1);

var SWIRcoefs = seqsTerms.map(function(term){
  var resul = seqsCoefs.map(function(coef){
    var resul2 = ee.String('NBR_coefs_').cat(ee.Number(term).format('%d'))
                                          .cat('_')
                                          .cat(ee.Number(coef).format('%d'));
    return resul2;
  });
  
  return ee.List(resul).flatten();
}).flatten();

print('SWIRcoefs', SWIRcoefs);

var otherProps = ee.List(['NBR_magnitude', 'NBR_rmse', 'changeProb',
                          'tBreak', 'tEnd', 'tStart']).map(function(prop){
  var resul = seqsTerms.map(function(term){
    var resul2 = ee.String(prop).cat('_').cat(ee.Number(term).format('%d'));
    return resul2;
  });
  
  return ee.List(resul).flatten();
}).flatten();

print('otherProps', otherProps);

var props = SWIRcoefs.add(otherProps).add('name').add('system:index').add('.geo').flatten();

print('props', props);

var varExp = flattenedResults.filter(ee.Filter.eq('name',idpt));
print('varExp', varExp);

Export.table.toDrive({
  collection: varExp,
  folder: folderDrive,
  description: 'fitted_id'+idpt+'_NBR_2000_2025_CCDCmetrics',
  fileFormat: 'CSV',
  selectors: props.getInfo()
  });
