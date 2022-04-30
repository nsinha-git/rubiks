package org.nish.HashDb;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.RandomAccessFile;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class HashDb {
    String dataDir;
    String keyFile;
    String dataFile;
    RandomAccessFile keyFileOutStream;




    public HashDb(String dataDir) throws FileNotFoundException {
        this.dataDir = dataDir;
        keyFile = dataDir + "/keyFile";
        dataFile = dataDir + "/dataFile";
        createSegments();

    }


    void createSegments() throws FileNotFoundException {
        Path keyFilePath = Paths.get(keyFile);

        if (Files.exists(keyFilePath)) {
             keyFileOutStream = new RandomAccessFile(keyFile,"a" );
        } else {
            keyFileOutStream = new RandomAccessFile(keyFile,"w" );

        }
    }



}
