# newDatabase, Jon Peder Lindemann, 24.02.2021
# Create a new occurrence SQLite database based on darwin core terminology
# Parameters:
# db_name | a string that specifies the filename of the new database

newDatabase <- function(db_name) {
  require(RSQLite)
  require(ISOcodes)
  
  # Open a new database connection
  CONN <- dbConnect(SQLite(), db_name)
  
  # Turn foreign keys on
  dbExecute(CONN,"PRAGMA foreign_keys = ON")
  
  # Country_codes
  dbExecute(CONN,"
           CREATE TABLE Country_codes (
           countryCode CHAR (2) NOT NULL,
           country VARCHAR (100),
           PRIMARY KEY (countryCode)
           )"
  )
  
  # Collecting  methods
  dbExecute(CONN,"
           CREATE TABLE Collecting_methods (
           ID CHAR (2) NOT NULL,
           samplingProtocol VARCHAR (20),
           PRIMARY KEY (ID)
           )"
  )
  
  # Collecting events
  dbExecute(CONN,"
           CREATE TABLE Collecting_events (
           eventID VARCHAR (50) NOT NULL,
           countryCode CHAR (2),
           stateProvince VARCHAR (50),
           county VARCHAR (50),
           strand_id CHAR (5),
           municipality CHAR (30),
           locality_1 VARCHAR (50),
           locality_2 VARCHAR (50),
           habitat VARCHAR (50),
           decimalLatitude DECIMAL (11,8),
           decimalLongitude DECIMAL(11,8),
           coordinateUncertaintyInMeters INT,
           samplingProtocol CHAR (2),
           eventDate_1 DATE,
           eventDate_2 DATE,
           recordedBy VARCHAR (50),
           eventRemarks VARCHAR (100),
           geodeticDatum VARCHAR (5),
           databased TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
           PRIMARY KEY (eventID),
           FOREIGN KEY (samplingProtocol)
              REFERENCES Collecting_methods(ID) ON UPDATE CASCADE,
           FOREIGN KEY (countryCode)
              REFERENCES Country_codes (countryCode)
           )"
  )
  
  # Rank
  dbExecute(CONN,"
           CREATE TABLE Taxon_ranks (
           taxonRank VARCHAR (20) NOT NULL,
           PRIMARY KEY (taxonRank)
           )"
  )
  # Taxa
  dbExecute(CONN,"
            CREATE TABLE Taxa (
            scientificName VARCHAR (100) NOT NULL,
            taxonRank VARCHAR (50) NOT NULL,
            scientificNameAuthorship VARCHAR (50),
            specificEpithet VARCHAR (50),
            genus VARCHAR (50),
            family VARCHAR (50),
            'order' VARCHAR (50),
            kingdom VARCHAR (50) DEFAULT 'animalia',
            phylum VARCHAR (50) DEFAULT 'arthropoda',
            class VARCHAR (50) DEFAULT 'insecta',
            nomenclaturalCode VARCHAR (10) DEFAULT 'ICZN',
            PRIMARY KEY (scientificName),
            FOREIGN KEY (taxonRank)
                REFERENCES Taxon_ranks(taxonRank) ON UPDATE CASCADE
            )"
  )
  
  # Datasets
  dbExecute(CONN, "
            CREATE TABLE Datasets (
            datasetName VARCHAR (150) NOT NULL,
            collectionCode VARCHAR (20),
            institutionCode VARCHAR (20),
            rightsHolder VARCHAR (150) NOT NULL,
            occurrenceID BLOB,
            PRIMARY KEY (datasetName)
            )"
  )
  
  # Projects
  dbExecute(CONN,"
           CREATE TABLE Projects (
           projectTitle VARCHAR (150),
           projectID VARCHAR (80),
           projectOwner VARCHAR (50),
           projectDescription VARCHAR (250),
           projectStartDate DATE,
           projectEndDate DATE,
           projectURL VARCHAR (150),
           occurrenceID BLOB,
           PRIMARY KEY (projectTitle)
           )"
  )
  
  # Sex
  dbExecute(CONN,"
           CREATE TABLE Sexes (
           sex VARCHAR (6) NOT NULL,
           PRIMARY KEY (sex)
           )"
  )
  
  # Life stages
  dbExecute(CONN,"
           CREATE TABLE Life_stages (
           lifeStage VARCHAR (5) NOT NULL,
           PRIMARY KEY (lifeStage)
           )"
  )
  
  # Occurrences 
  dbExecute(CONN, "
            CREATE TABLE Occurrences (
            occurrenceID VARCHAR (80) NOT NULL,
            eventID VARCHAR (20),
            scientificName VARCHAR (50),
            identifiedBy VARCHAR (100),
            individualCount INT,
            sex VARCHAR (6),
            lifeStage VARCHAR (5), 
            preparations VARCHAR (50),
            occurrenceRemarks VARCHAR (100),
            associatedTaxa VARCHAR (100),
            associatedReferences VARCHAR (100),
            unit_id VARCHAR (80),
            ownerInstitutionCode VARCHAR (20),
            dateIdentified DATE DEFAULT CURRENT_DATE,
            databased TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            last_export TIMESTAMP,
            PRIMARY KEY (occurrenceID),
            FOREIGN KEY (eventID)
                  REFERENCES Collecting_events (eventID) ON UPDATE RESTRICT ON DELETE CASCADE,
            FOREIGN KEY (scientificName)
                  REFERENCES Taxa(scientificName) ON UPDATE CASCADE,
            FOREIGN KEY (sex)
                  REFERENCES Sexes (sex) ON UPDATE CASCADE,
            FOREIGN KEY (lifeStage)
                  REFERENCES Life_stages (lifeStage) ON UPDATE CASCADE
            )"
  )
  
  # Populate the simplest parent keys
  ## country codes
  iso_3166_1 <- ISOcodes::ISO_3166_1[, c(1,4)]
  names(iso_3166_1) <- c("countryCode", "country")
  ## methods 
  met <- data.frame(ID = c("sn", "mt", "wt", "pt", "yp", "wp", "bp", "it", "st", "hp", "rd", "lt", "pf"), samplingProtocol = c("Sweep-net", "Malaise-trap", "Window-trap", "Pan-trap", "Yellow pan", "White pan", "Blue pan", "Intercept-trap", "Slam-trap", "Hand-picked", "Reared", "Light-trap", "Pitfall-trap"))
  ## ranks
  rank <- data.frame(taxonRank = c("species", "species-group", "genus", "tribe", "subfamily", "family", "superfamily", "infraorder", "order", "class", "spnov", "other"))
  ## sexes
  sex <- data.frame(sex = c("male", "female", ""))
  ## lifestages
  life_stage <- data.frame(lifeStage = c("larva", "pupa", "adult", ""))
  
  # Add data to tables
  dbAppendTable(CONN, "Country_codes", iso_3166_1)
  dbAppendTable(CONN, "Collecting_methods", met)
  dbAppendTable(CONN, "Taxon_ranks", rank)
  dbAppendTable(CONN, "Sexes", sex)
  dbAppendTable(CONN, "Life_stages", life_stage)
  
  # Dissconnect
  dbDisconnect(CONN)
}
