       01  INQMFECI.
           02  FILLER PIC X(12).
           02  TRANIDL    COMP  PIC  S9(4).
           02  TRANIDF    PICTURE X.
           02  FILLER REDEFINES TRANIDF.
             03 TRANIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TRANIDI  PIC X(4).
           02  CUSTNOL    COMP  PIC  S9(4).
           02  CUSTNOF    PICTURE X.
           02  FILLER REDEFINES CUSTNOF.
             03 CUSTNOA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CUSTNOI  PIC X(6).
           02  LNAMEL    COMP  PIC  S9(4).
           02  LNAMEF    PICTURE X.
           02  FILLER REDEFINES LNAMEF.
             03 LNAMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LNAMEI  PIC X(30).
           02  FNAMEL    COMP  PIC  S9(4).
           02  FNAMEF    PICTURE X.
           02  FILLER REDEFINES FNAMEF.
             03 FNAMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  FNAMEI  PIC X(20).
           02  ADDRL    COMP  PIC  S9(4).
           02  ADDRF    PICTURE X.
           02  FILLER REDEFINES ADDRF.
             03 ADDRA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ADDRI  PIC X(30).
           02  CITYL    COMP  PIC  S9(4).
           02  CITYF    PICTURE X.
           02  FILLER REDEFINES CITYF.
             03 CITYA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CITYI  PIC X(20).
           02  STATEL    COMP  PIC  S9(4).
           02  STATEF    PICTURE X.
           02  FILLER REDEFINES STATEF.
             03 STATEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  STATEI  PIC X(2).
           02  ZIPCODEL    COMP  PIC  S9(4).
           02  ZIPCODEF    PICTURE X.
           02  FILLER REDEFINES ZIPCODEF.
             03 ZIPCODEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ZIPCODEI  PIC X(10).
           02  MESSAGEL    COMP  PIC  S9(4).
           02  MESSAGEF    PICTURE X.
           02  FILLER REDEFINES MESSAGEF.
             03 MESSAGEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MESSAGEI  PIC X(79).
           02  DUMMYL    COMP  PIC  S9(4).
           02  DUMMYF    PICTURE X.
           02  FILLER REDEFINES DUMMYF.
             03 DUMMYA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DUMMYI  PIC X(1).
       01  INQMFECO REDEFINES INQMFECI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANIDC    PICTURE X.
           02  TRANIDH    PICTURE X.
           02  TRANIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  CUSTNOC    PICTURE X.
           02  CUSTNOH    PICTURE X.
           02  CUSTNOO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  LNAMEC    PICTURE X.
           02  LNAMEH    PICTURE X.
           02  LNAMEO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  FNAMEC    PICTURE X.
           02  FNAMEH    PICTURE X.
           02  FNAMEO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  ADDRC    PICTURE X.
           02  ADDRH    PICTURE X.
           02  ADDRO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  CITYC    PICTURE X.
           02  CITYH    PICTURE X.
           02  CITYO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  STATEC    PICTURE X.
           02  STATEH    PICTURE X.
           02  STATEO  PIC X(2).
           02  FILLER PICTURE X(3).
           02  ZIPCODEC    PICTURE X.
           02  ZIPCODEH    PICTURE X.
           02  ZIPCODEO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  MESSAGEC    PICTURE X.
           02  MESSAGEH    PICTURE X.
           02  MESSAGEO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  DUMMYC    PICTURE X.
           02  DUMMYH    PICTURE X.
           02  DUMMYO  PIC X(1).