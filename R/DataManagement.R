

#' Converts ICES csv to ICES XML
#'
#' This function converts ices csv files into ices xml files
#' @param ices_csv The temperature in Fahrenheit.
#' @export

ICEScsv2xml <- function(ices_csv = NULL){
  warning('This function should never be used to create data for official usage')

  data <- read.csv(file = ices_csv,sep=',',header = F)

  data_instrument <- as.data.frame(data[data[,1]=='Instrument',])
  names(data_instrument) <- data_instrument[1, ]
  data_instrument <- data_instrument[-1, ]
  data_calibration<- data[data[,1]=='Calibration',]
  names(data_calibration) <- data_calibration[1, ]
  data_calibration <- data_calibration[-1, ]

  data_DataAcquisition<- data[data[,1]=='DataAcquisition',]
  names(data_DataAcquisition) <- data_DataAcquisition[1, ]
  data_DataAcquisition <- data_DataAcquisition[-1, ]

  data_DataProcessing<- data[data[,1]=='DataProcessing',]
  names(data_DataProcessing) <- data_DataProcessing[1, ]
  data_DataProcessing <- data_DataProcessing[-1, ]

  data_Cruise<- data[data[,1]=='Cruise',]
  names(data_Cruise) <- data_Cruise[1, ]
  data_Cruise <- data_Cruise[-1, ]

  data_Data<- data[data[,1]=='Data',]
  names(data_Data) <- data_Data[1, ]
  data_Data <- data_Data[-1, ]


  library(XML)

  # XML STRING
  prefix.xml <- "<Acoustic />"


  # BUILD XML TREE
  doc = xmlTreeParse(prefix.xml, useInternalNodes = T)     # PARSE STRING
  root = xmlRoot(doc)                                      # FIND ROOT


  for(i in 1:nrow(data_instrument)){
    df_line = data_instrument[i,]

    reportNode = newXMLNode("Instrument", parent=root)           # ADD TO ROOT
    xmlAttrs(reportNode) = c(ID = df_line$InstrumentID)              # ADD ATTRIBUTE
    newXMLNode = newXMLNode("Frequency",df_line$InstrumentFrequency, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransducerLocation", parent=reportNode)
    xmlAttrs(newXMLNode) = c(IDREF = df_line$InstrumentTransducerLocation)
    newXMLNode = newXMLNode("TransceiverManufacturer",df_line$InstrumentTransceiverManufacturer, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransducerModel",df_line$InstrumentTransducerModel, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransducerSerial",df_line$InstrumentTransducerSerial, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransducerBeamType",parent=reportNode)
    xmlAttrs(newXMLNode) = c(IDREF = df_line$InstrumentTransducerBeamType)         # ADD TO REPORT
    newXMLNode = newXMLNode("TransducerDepth",df_line$InstrumentTransducerDepth, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransducerOrientation",df_line$InstrumentTransducerOrientation, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransducerPSI",df_line$InstrumentTransducerPSI, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransducerBeamAngleMajor",df_line$InstrumentTransducerBeamAngleMajor, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransducerBeamAngleMinor",df_line$InstrumentTransducerBeamAngleMinor, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransceiverManufacturer",df_line$InstrumentTransceiverManufacturer, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransceiverModel",df_line$InstrumentTransceiverModel, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransceiverSerial",df_line$InstrumentTransceiverSerial, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("TransceiverFirmware",df_line$InstrumentTransceiverFirmware, parent=reportNode)       # ADD TO REPORT
  }

  for(i in 1:nrow(data_calibration)){
    df_line = data_calibration[i,]

    reportNode = newXMLNode("Calibration", parent=root)           # ADD TO ROOT
    xmlAttrs(reportNode) = c(ID = df_line$CalibrationID)              # ADD ATTRIBUTE
    newXMLNode = newXMLNode("Data",df_line$CalibrationDate, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("AcquisitionMethod",parent=reportNode)
    xmlAttrs(newXMLNode) = c(IDREF = df_line$CalibrationAcquisitionMethod)
    newXMLNode = newXMLNode("ProcessingMethod", parent=reportNode)
    xmlAttrs(newXMLNode) = c(IDREF = df_line$CalibrationProcessingMethod)
    newXMLNode = newXMLNode("AccuracyEstimate",df_line$CalibrationAccuracyEstimate, parent=reportNode)       # ADD TO REPORT
  }


  for(i in 1:nrow(data_DataAcquisition)){
    df_line = data_DataAcquisition[i,]

    reportNode = newXMLNode("DataAcquisition", parent=root)           # ADD TO ROOT
    xmlAttrs(reportNode) = c(ID = df_line$DataAcquisitionID)              # ADD ATTRIBUTE
    newXMLNode = newXMLNode("SoftwareName",parent=reportNode)
    xmlAttrs(newXMLNode) = c(IDREF = df_line$DataAcquisitionSoftwareName)
    newXMLNode = newXMLNode("SoftwareVersion",df_line$DataAcquisitionSoftwareVersion, parent=reportNode)       # ADD TO REPORT
    newXMLNode = newXMLNode("StoredDataFormat", parent=reportNode)       # ADD TO REPORT        # ADD TO ROOT
    xmlAttrs(newXMLNode) = c(IDREF = df_line$DataAcquisitionStoredDataFormat)
    newXMLNode = newXMLNode("PingDutyCycle",df_line$DataAcquisitionPingDutyCycle, parent=reportNode)       # ADD TO REPORT
  }


  for(i in 1:nrow(data_DataProcessing)){
    df_line = data_DataProcessing[i,]

    reportNode = newXMLNode("DataProcessing", parent=root)           # ADD TO ROOT
    xmlAttrs(reportNode) = c(ID = df_line$DataProcessingID)
    newXMLNode = newXMLNode("SoftwareName", parent=reportNode)           # ADD TO ROOT
    xmlAttrs(newXMLNode) = c(ID = df_line$DataProcessingSoftwareName)              # ADD ATTRIBUTE
    newXMLNode = newXMLNode("SoftwareVersion",df_line$DataProcessingSoftwareVersion,parent=reportNode)
    newXMLNode = newXMLNode("TriwaveCorrection", parent=reportNode)
    xmlAttrs(newXMLNode) = c(ID = df_line$DataProcessingTriwaveCorrection)
    newXMLNode = newXMLNode("ChannelID",df_line$DataProcessingChannelID,parent=reportNode)
    newXMLNode = newXMLNode("Bandwidth",df_line$DataProcessingBandwidth,parent=reportNode)
    newXMLNode = newXMLNode("Frequency",df_line$DataProcessingFrequency,parent=reportNode)
    newXMLNode = newXMLNode("TransceiverPower",df_line$DataProcessingTransceiverPower,parent=reportNode)
    newXMLNode = newXMLNode("TransmitPulseLength",df_line$DataProcessingTransmitPulseLength,parent=reportNode)
    newXMLNode = newXMLNode("OnAxisGain",df_line$DataProcessingOnAxisGain,parent=reportNode)
    newXMLNode = newXMLNode("OnAxisGainUnit", parent=reportNode)
    xmlAttrs(newXMLNode) = c(ID = df_line$DataProcessingOnAxisGainUnit)
    newXMLNode = newXMLNode("SaCorrection",df_line$DataProcessingSaCorrection,parent=reportNode)
    newXMLNode = newXMLNode("Absorption",df_line$DataProcessingAbsorption,parent=reportNode)
    newXMLNode = newXMLNode("AbsorptionDescription",df_line$DataProcessingAbsorptionDescription,parent=reportNode)
    newXMLNode = newXMLNode("SoundSpeed",df_line$DataProcessingSoundSpeed,parent=reportNode)
    newXMLNode = newXMLNode("SoundSpeedDescription",df_line$DataProcessingSoundSpeedDescription,parent=reportNode)
    newXMLNode = newXMLNode("TransducerPSI",df_line$DataProcessingTransducerPSI,parent=reportNode)
  }


  VocabularyNode = newXMLNode("Vocabulary", parent=root)
  reportNode = newXMLNode("Survey", parent=VocabularyNode)
  newNode = newXMLNode("Code", data_Cruise$CruiseSurvey,parent=reportNode)
  xmlAttrs(newNode) = c(ID = data_Cruise$CruiseSurvey)
  reportNode = newXMLNode("Country", parent=VocabularyNode)
  newNode = newXMLNode("Code", data_Cruise$CruiseCountry,parent=reportNode)
  xmlAttrs(newNode) = c(ID = data_Cruise$CruiseCountry)
  reportNode = newXMLNode("Platform", parent=VocabularyNode)
  newNode = newXMLNode("Code", data_Cruise$CruisePlatform,parent=reportNode)
  xmlAttrs(newNode) = c(ID = data_Cruise$CruisePlatform)
  reportNode = newXMLNode("Organisation", parent=VocabularyNode)
  newNode = newXMLNode("Code", data_Cruise$CruiseOrganisation,parent=reportNode)
  xmlAttrs(newNode) = c(ID = data_Cruise$CruiseOrganisation)
  reportNode = newXMLNode("Origin", parent=VocabularyNode)
  newNode = newXMLNode("Code", unique(data_Data$LogOrigin),parent=reportNode)
  xmlAttrs(newNode) = c(ID = unique(data_Data$LogOrigin))
  reportNode = newXMLNode("LogValidity", parent=VocabularyNode)
  newNode = newXMLNode("Code", unique(data_Data$LogValidity),parent=reportNode)
  xmlAttrs(newNode) = c(ID = unique(data_Data$LogValidity))

  reportNode = newXMLNode("SaCategory", parent=VocabularyNode)
  for(sa in unique(data_Data$DataSaCategory)){
    newNode = newXMLNode("Code", sa,parent=reportNode)
    xmlAttrs(newNode) = c(ID = sa)
  }

  reportNode = newXMLNode("Type", parent=VocabularyNode)
  newNode = newXMLNode("Code", unique(data_Data$DataType),parent=reportNode)
  xmlAttrs(newNode) = c(ID = unique(data_Data$DataType))
  reportNode = newXMLNode("Unit", parent=VocabularyNode)
  newNode = newXMLNode("Code", unique(data_Data$DataUnit),parent=reportNode)
  xmlAttrs(newNode) = c(ID = unique(data_Data$DataUnit))


  reportNode = newXMLNode("Cruise", parent=root)

  surveyNode = newXMLNode("Survey", parent=reportNode)
  for(a in data_Cruise$CruiseSurvey){
    newXMLNode = newXMLNode("Code", parent=reportNode)
    xmlAttrs(newXMLNode) = c(IDREF = a)
  }

  CountryNode = newXMLNode("Country", parent=reportNode)
  xmlAttrs(CountryNode) = c(IDREF = unique(data_Cruise$CruiseCountry))
  CountryNode = newXMLNode("Platform", parent=reportNode)
  xmlAttrs(CountryNode) = c(IDREF = unique(data_Cruise$CruisePlatform))
  CountryNode = newXMLNode("StartDate", unique(data_Cruise$CruiseStartDate),parent=reportNode)
  CountryNode = newXMLNode("EndDate", unique(data_Cruise$CruiseEndDate),parent=reportNode)
  CountryNode = newXMLNode("Organisation", unique(data_Cruise$CruiseOrganisation),parent=reportNode)
  CountryNode = newXMLNode("LocalID", unique(data_Cruise$CruiseLocalID),parent=reportNode)

  iii=1
  for(log in unique(data_Data$LogDistance)){
    print(log)
    LogNode = newXMLNode("Log", parent=reportNode)

    df_line = data_Data[data_Data$LogDistance==log,]


    newNode = newXMLNode("Distance", as.numeric(log),parent=LogNode)
    newNode = newXMLNode("Time", unique(df_line$LogTime),parent=LogNode)
    newNode = newXMLNode("Latitude", unique(df_line$LogLatitude),parent=LogNode)
    newNode = newXMLNode("Longitude", unique(df_line$LogLongitude),parent=LogNode)
    newNode = newXMLNode("Origin", parent=LogNode)
    xmlAttrs(newNode) = c(IDREF = unique(df_line$LogOrigin))
    newNode = newXMLNode("Validity", parent=LogNode)
    xmlAttrs(newNode) = c(IDREF = unique(df_line$LogValidity))
    newNode = newXMLNode("BottomDepth", unique(df_line$LogBottomDepth),parent=LogNode)

    for(depth in unique(df_line$SampleChannelDepthUpper)){
      SampleNode = newXMLNode("Sample", parent=LogNode)
      sample <- df_line[df_line$SampleChannelDepthLower==depth,]

      newNode = newXMLNode("ChannelDepthUpper", as.numeric(unique(sample$SampleChannelDepthUpper)),parent=SampleNode)
      newNode = newXMLNode("ChannelDepthLower", as.numeric(unique(sample$SampleChannelDepthLower)),parent=SampleNode)
      newNode = newXMLNode("PingAxisInterval", as.numeric(unique(sample$SamplePingAxisInterval)),parent=SampleNode)
      newNode = newXMLNode("PingAxisIntervalType", parent=SampleNode)
      xmlAttrs(newNode) = c(IDREF = (unique(sample$SamplePingAxisIntervalType)))
      newNode = newXMLNode("PingAxisIntervalUnit", parent=SampleNode)
      xmlAttrs(newNode) = c(IDREF = (unique(sample$SamplePingAxisIntervalUnit)))
      newNode = newXMLNode("SvThreshold", as.numeric(unique(sample$SampleSvThreshold)),parent=SampleNode)
      newNode = newXMLNode("Instrument", parent=SampleNode)
      xmlAttrs(newNode) = c(IDREF = (unique(sample$Instrument)))
      newNode = newXMLNode("Calibration", parent=SampleNode)
      xmlAttrs(newNode) = c(IDREF = (unique(sample$Calibration)))
      newNode = newXMLNode("DataAcquisition", parent=SampleNode)
      xmlAttrs(newNode) = c(IDREF = (unique(sample$DataAcquisition)))
      newNode = newXMLNode("DataProcessing", parent=SampleNode)
      xmlAttrs(newNode) = c(IDREF = (unique(sample$DataProcessing)))
      newNode = newXMLNode("PingAxisIntervalOrigin", parent=SampleNode)
      xmlAttrs(newNode) = c(IDREF = (unique(sample$SamplePingAxisIntervalOrigin)))

      for( i in 1:nrow(sample)){
        Data<-sample[i,]
        DataNode = newXMLNode("Data", parent=SampleNode)

        newNode = newXMLNode("SaCategory", parent=DataNode)
        xmlAttrs(newNode) = c(IDREF = (unique(Data$DataSaCategory)))
        newNode = newXMLNode("Type", parent=DataNode)
        xmlAttrs(newNode) = c(IDREF = (unique(Data$DataType)))
        newNode = newXMLNode("Unit", parent=DataNode)
        xmlAttrs(newNode) = c(IDREF = (unique(Data$DataUnit)))
        newNode = newXMLNode("Value", as.numeric(Data$DataValue),parent=DataNode)
      }
    }
  }


  files <- list.files(pattern="*.csv")
  newfiles <- gsub(".csv$", ".xml", ices_csv)

  saveXML(doc, file=newfiles)
}


