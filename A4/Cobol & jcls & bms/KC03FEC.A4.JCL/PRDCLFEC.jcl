//PRDCLFEC  JOB
/*
/* FIX NAME, DATA NAME, AND INDEX NAME TO MATCH NAMING CONVENTIONS
/* FIX KEYS, AND RECSZ TO MATCH RECORD LAYOUT REQUIRED
/* FIX KC03YYY TO YOUR USERID KC03XXX
/* FIX CMFYYY TO YOUR USERID PATTERN I.E. CMFXXX
/*
//DEFCLST EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
   DEFINE CLUSTER ( -
   NAME(KC03FEC.A4.CICS.PRDFEC)  -
   INDEXED                                 -
   KEYS(10 0)                             -
   RECSZ(46 46)                            -
   TRACKS(1,1)                             -
   CISZ(4096)                              -
   FREESPACE(3 3) )                        -
   DATA (NAME(KC03FEC.A4.CICS.PRDFEC.DATA))      -
   INDEX (NAME(KC03FEC.A4.CICS.PRDFEC.INDEX))
/*