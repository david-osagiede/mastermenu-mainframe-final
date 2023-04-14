       01  ORDMFECI.
           02  FILLER PIC X(12).
           02  TRANIDL    COMP  PIC  S9(4).
           02  TRANIDF    PICTURE X.
           02  FILLER REDEFINES TRANIDF.
             03 TRANIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TRANIDI  PIC X(4).
           02  INSTRL    COMP  PIC  S9(4).
           02  INSTRF    PICTURE X.
           02  FILLER REDEFINES INSTRF.
             03 INSTRA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  INSTRI  PIC X(79).
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
           02  POL    COMP  PIC  S9(4).
           02  POF    PICTURE X.
           02  FILLER REDEFINES POF.
             03 POA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  POI  PIC X(10).
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
           02  PCODE1L    COMP  PIC  S9(4).
           02  PCODE1F    PICTURE X.
           02  FILLER REDEFINES PCODE1F.
             03 PCODE1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE1I  PIC X(10).
           02  QTY1L    COMP  PIC  S9(4).
           02  QTY1F    PICTURE X.
           02  FILLER REDEFINES QTY1F.
             03 QTY1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY1I  PIC X(5).
           02  DESC1L    COMP  PIC  S9(4).
           02  DESC1F    PICTURE X.
           02  FILLER REDEFINES DESC1F.
             03 DESC1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC1I  PIC X(20).
           02  LIST1L    COMP  PIC  S9(4).
           02  LIST1F    PICTURE X.
           02  FILLER REDEFINES LIST1F.
             03 LIST1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST1I  PIC X(12).
           02  NET1L    COMP  PIC  S9(4).
           02  NET1F    PICTURE X.
           02  FILLER REDEFINES NET1F.
             03 NET1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET1I  PIC X(10).
           02  AMT1L    COMP  PIC  S9(4).
           02  AMT1F    PICTURE X.
           02  FILLER REDEFINES AMT1F.
             03 AMT1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT1I  PIC X(12).
           02  PCODE2L    COMP  PIC  S9(4).
           02  PCODE2F    PICTURE X.
           02  FILLER REDEFINES PCODE2F.
             03 PCODE2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE2I  PIC X(10).
           02  QTY2L    COMP  PIC  S9(4).
           02  QTY2F    PICTURE X.
           02  FILLER REDEFINES QTY2F.
             03 QTY2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY2I  PIC X(5).
           02  DESC2L    COMP  PIC  S9(4).
           02  DESC2F    PICTURE X.
           02  FILLER REDEFINES DESC2F.
             03 DESC2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC2I  PIC X(20).
           02  LIST2L    COMP  PIC  S9(4).
           02  LIST2F    PICTURE X.
           02  FILLER REDEFINES LIST2F.
             03 LIST2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST2I  PIC X(12).
           02  NET2L    COMP  PIC  S9(4).
           02  NET2F    PICTURE X.
           02  FILLER REDEFINES NET2F.
             03 NET2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET2I  PIC X(10).
           02  AMT2L    COMP  PIC  S9(4).
           02  AMT2F    PICTURE X.
           02  FILLER REDEFINES AMT2F.
             03 AMT2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT2I  PIC X(12).
           02  PCODE3L    COMP  PIC  S9(4).
           02  PCODE3F    PICTURE X.
           02  FILLER REDEFINES PCODE3F.
             03 PCODE3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE3I  PIC X(10).
           02  QTY3L    COMP  PIC  S9(4).
           02  QTY3F    PICTURE X.
           02  FILLER REDEFINES QTY3F.
             03 QTY3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY3I  PIC X(5).
           02  DESC3L    COMP  PIC  S9(4).
           02  DESC3F    PICTURE X.
           02  FILLER REDEFINES DESC3F.
             03 DESC3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC3I  PIC X(20).
           02  LIST3L    COMP  PIC  S9(4).
           02  LIST3F    PICTURE X.
           02  FILLER REDEFINES LIST3F.
             03 LIST3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST3I  PIC X(12).
           02  NET3L    COMP  PIC  S9(4).
           02  NET3F    PICTURE X.
           02  FILLER REDEFINES NET3F.
             03 NET3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET3I  PIC X(10).
           02  AMT3L    COMP  PIC  S9(4).
           02  AMT3F    PICTURE X.
           02  FILLER REDEFINES AMT3F.
             03 AMT3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT3I  PIC X(12).
           02  PCODE4L    COMP  PIC  S9(4).
           02  PCODE4F    PICTURE X.
           02  FILLER REDEFINES PCODE4F.
             03 PCODE4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE4I  PIC X(10).
           02  QTY4L    COMP  PIC  S9(4).
           02  QTY4F    PICTURE X.
           02  FILLER REDEFINES QTY4F.
             03 QTY4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY4I  PIC X(5).
           02  DESC4L    COMP  PIC  S9(4).
           02  DESC4F    PICTURE X.
           02  FILLER REDEFINES DESC4F.
             03 DESC4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC4I  PIC X(20).
           02  LIST4L    COMP  PIC  S9(4).
           02  LIST4F    PICTURE X.
           02  FILLER REDEFINES LIST4F.
             03 LIST4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST4I  PIC X(12).
           02  NET4L    COMP  PIC  S9(4).
           02  NET4F    PICTURE X.
           02  FILLER REDEFINES NET4F.
             03 NET4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET4I  PIC X(10).
           02  AMT4L    COMP  PIC  S9(4).
           02  AMT4F    PICTURE X.
           02  FILLER REDEFINES AMT4F.
             03 AMT4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT4I  PIC X(12).
           02  PCODE5L    COMP  PIC  S9(4).
           02  PCODE5F    PICTURE X.
           02  FILLER REDEFINES PCODE5F.
             03 PCODE5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE5I  PIC X(10).
           02  QTY5L    COMP  PIC  S9(4).
           02  QTY5F    PICTURE X.
           02  FILLER REDEFINES QTY5F.
             03 QTY5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY5I  PIC X(5).
           02  DESC5L    COMP  PIC  S9(4).
           02  DESC5F    PICTURE X.
           02  FILLER REDEFINES DESC5F.
             03 DESC5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC5I  PIC X(20).
           02  LIST5L    COMP  PIC  S9(4).
           02  LIST5F    PICTURE X.
           02  FILLER REDEFINES LIST5F.
             03 LIST5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST5I  PIC X(12).
           02  NET5L    COMP  PIC  S9(4).
           02  NET5F    PICTURE X.
           02  FILLER REDEFINES NET5F.
             03 NET5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET5I  PIC X(10).
           02  AMT5L    COMP  PIC  S9(4).
           02  AMT5F    PICTURE X.
           02  FILLER REDEFINES AMT5F.
             03 AMT5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT5I  PIC X(12).
           02  PCODE6L    COMP  PIC  S9(4).
           02  PCODE6F    PICTURE X.
           02  FILLER REDEFINES PCODE6F.
             03 PCODE6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE6I  PIC X(10).
           02  QTY6L    COMP  PIC  S9(4).
           02  QTY6F    PICTURE X.
           02  FILLER REDEFINES QTY6F.
             03 QTY6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY6I  PIC X(5).
           02  DESC6L    COMP  PIC  S9(4).
           02  DESC6F    PICTURE X.
           02  FILLER REDEFINES DESC6F.
             03 DESC6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC6I  PIC X(20).
           02  LIST6L    COMP  PIC  S9(4).
           02  LIST6F    PICTURE X.
           02  FILLER REDEFINES LIST6F.
             03 LIST6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST6I  PIC X(12).
           02  NET6L    COMP  PIC  S9(4).
           02  NET6F    PICTURE X.
           02  FILLER REDEFINES NET6F.
             03 NET6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET6I  PIC X(10).
           02  AMT6L    COMP  PIC  S9(4).
           02  AMT6F    PICTURE X.
           02  FILLER REDEFINES AMT6F.
             03 AMT6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT6I  PIC X(12).
           02  PCODE7L    COMP  PIC  S9(4).
           02  PCODE7F    PICTURE X.
           02  FILLER REDEFINES PCODE7F.
             03 PCODE7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE7I  PIC X(10).
           02  QTY7L    COMP  PIC  S9(4).
           02  QTY7F    PICTURE X.
           02  FILLER REDEFINES QTY7F.
             03 QTY7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY7I  PIC X(5).
           02  DESC7L    COMP  PIC  S9(4).
           02  DESC7F    PICTURE X.
           02  FILLER REDEFINES DESC7F.
             03 DESC7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC7I  PIC X(20).
           02  LIST7L    COMP  PIC  S9(4).
           02  LIST7F    PICTURE X.
           02  FILLER REDEFINES LIST7F.
             03 LIST7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST7I  PIC X(12).
           02  NET7L    COMP  PIC  S9(4).
           02  NET7F    PICTURE X.
           02  FILLER REDEFINES NET7F.
             03 NET7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET7I  PIC X(10).
           02  AMT7L    COMP  PIC  S9(4).
           02  AMT7F    PICTURE X.
           02  FILLER REDEFINES AMT7F.
             03 AMT7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT7I  PIC X(12).
           02  PCODE8L    COMP  PIC  S9(4).
           02  PCODE8F    PICTURE X.
           02  FILLER REDEFINES PCODE8F.
             03 PCODE8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE8I  PIC X(10).
           02  QTY8L    COMP  PIC  S9(4).
           02  QTY8F    PICTURE X.
           02  FILLER REDEFINES QTY8F.
             03 QTY8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY8I  PIC X(5).
           02  DESC8L    COMP  PIC  S9(4).
           02  DESC8F    PICTURE X.
           02  FILLER REDEFINES DESC8F.
             03 DESC8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC8I  PIC X(20).
           02  LIST8L    COMP  PIC  S9(4).
           02  LIST8F    PICTURE X.
           02  FILLER REDEFINES LIST8F.
             03 LIST8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST8I  PIC X(12).
           02  NET8L    COMP  PIC  S9(4).
           02  NET8F    PICTURE X.
           02  FILLER REDEFINES NET8F.
             03 NET8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET8I  PIC X(10).
           02  AMT8L    COMP  PIC  S9(4).
           02  AMT8F    PICTURE X.
           02  FILLER REDEFINES AMT8F.
             03 AMT8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT8I  PIC X(12).
           02  PCODE9L    COMP  PIC  S9(4).
           02  PCODE9F    PICTURE X.
           02  FILLER REDEFINES PCODE9F.
             03 PCODE9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE9I  PIC X(10).
           02  QTY9L    COMP  PIC  S9(4).
           02  QTY9F    PICTURE X.
           02  FILLER REDEFINES QTY9F.
             03 QTY9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY9I  PIC X(5).
           02  DESC9L    COMP  PIC  S9(4).
           02  DESC9F    PICTURE X.
           02  FILLER REDEFINES DESC9F.
             03 DESC9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC9I  PIC X(20).
           02  LIST9L    COMP  PIC  S9(4).
           02  LIST9F    PICTURE X.
           02  FILLER REDEFINES LIST9F.
             03 LIST9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST9I  PIC X(12).
           02  NET9L    COMP  PIC  S9(4).
           02  NET9F    PICTURE X.
           02  FILLER REDEFINES NET9F.
             03 NET9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET9I  PIC X(10).
           02  AMT9L    COMP  PIC  S9(4).
           02  AMT9F    PICTURE X.
           02  FILLER REDEFINES AMT9F.
             03 AMT9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT9I  PIC X(12).
           02  PCODE10L    COMP  PIC  S9(4).
           02  PCODE10F    PICTURE X.
           02  FILLER REDEFINES PCODE10F.
             03 PCODE10A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  PCODE10I  PIC X(10).
           02  QTY10L    COMP  PIC  S9(4).
           02  QTY10F    PICTURE X.
           02  FILLER REDEFINES QTY10F.
             03 QTY10A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  QTY10I  PIC X(5).
           02  DESC10L    COMP  PIC  S9(4).
           02  DESC10F    PICTURE X.
           02  FILLER REDEFINES DESC10F.
             03 DESC10A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DESC10I  PIC X(20).
           02  LIST10L    COMP  PIC  S9(4).
           02  LIST10F    PICTURE X.
           02  FILLER REDEFINES LIST10F.
             03 LIST10A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  LIST10I  PIC X(12).
           02  NET10L    COMP  PIC  S9(4).
           02  NET10F    PICTURE X.
           02  FILLER REDEFINES NET10F.
             03 NET10A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NET10I  PIC X(10).
           02  AMT10L    COMP  PIC  S9(4).
           02  AMT10F    PICTURE X.
           02  FILLER REDEFINES AMT10F.
             03 AMT10A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMT10I  PIC X(12).
           02  TOTALL    COMP  PIC  S9(4).
           02  TOTALF    PICTURE X.
           02  FILLER REDEFINES TOTALF.
             03 TOTALA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TOTALI  PIC X(12).
           02  MSGL    COMP  PIC  S9(4).
           02  MSGF    PICTURE X.
           02  FILLER REDEFINES MSGF.
             03 MSGA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGI  PIC X(79).
           02  FKEYL    COMP  PIC  S9(4).
           02  FKEYF    PICTURE X.
           02  FILLER REDEFINES FKEYF.
             03 FKEYA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  FKEYI  PIC X(40).
           02  DUMMYL    COMP  PIC  S9(4).
           02  DUMMYF    PICTURE X.
           02  FILLER REDEFINES DUMMYF.
             03 DUMMYA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  DUMMYI  PIC X(1).
       01  ORDMFECO REDEFINES ORDMFECI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANIDC    PICTURE X.
           02  TRANIDH    PICTURE X.
           02  TRANIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  INSTRC    PICTURE X.
           02  INSTRH    PICTURE X.
           02  INSTRO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  CUSTNOC    PICTURE X.
           02  CUSTNOH    PICTURE X.
           02  CUSTNOO  PIC X(6).
           02  FILLER PICTURE X(3).
           02  LNAMEC    PICTURE X.
           02  LNAMEH    PICTURE X.
           02  LNAMEO  PIC X(30).
           02  FILLER PICTURE X(3).
           02  POC    PICTURE X.
           02  POH    PICTURE X.
           02  POO  PIC X(10).
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
           02  PCODE1C    PICTURE X.
           02  PCODE1H    PICTURE X.
           02  PCODE1O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY1C    PICTURE X.
           02  QTY1H    PICTURE X.
           02  QTY1O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC1C    PICTURE X.
           02  DESC1H    PICTURE X.
           02  DESC1O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST1C    PICTURE X.
           02  LIST1H    PICTURE X.
           02  LIST1O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET1C    PICTURE X.
           02  NET1H    PICTURE X.
           02  NET1O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT1C    PICTURE X.
           02  AMT1H    PICTURE X.
           02  AMT1O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  PCODE2C    PICTURE X.
           02  PCODE2H    PICTURE X.
           02  PCODE2O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY2C    PICTURE X.
           02  QTY2H    PICTURE X.
           02  QTY2O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC2C    PICTURE X.
           02  DESC2H    PICTURE X.
           02  DESC2O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST2C    PICTURE X.
           02  LIST2H    PICTURE X.
           02  LIST2O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET2C    PICTURE X.
           02  NET2H    PICTURE X.
           02  NET2O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT2C    PICTURE X.
           02  AMT2H    PICTURE X.
           02  AMT2O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  PCODE3C    PICTURE X.
           02  PCODE3H    PICTURE X.
           02  PCODE3O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY3C    PICTURE X.
           02  QTY3H    PICTURE X.
           02  QTY3O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC3C    PICTURE X.
           02  DESC3H    PICTURE X.
           02  DESC3O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST3C    PICTURE X.
           02  LIST3H    PICTURE X.
           02  LIST3O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET3C    PICTURE X.
           02  NET3H    PICTURE X.
           02  NET3O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT3C    PICTURE X.
           02  AMT3H    PICTURE X.
           02  AMT3O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  PCODE4C    PICTURE X.
           02  PCODE4H    PICTURE X.
           02  PCODE4O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY4C    PICTURE X.
           02  QTY4H    PICTURE X.
           02  QTY4O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC4C    PICTURE X.
           02  DESC4H    PICTURE X.
           02  DESC4O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST4C    PICTURE X.
           02  LIST4H    PICTURE X.
           02  LIST4O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET4C    PICTURE X.
           02  NET4H    PICTURE X.
           02  NET4O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT4C    PICTURE X.
           02  AMT4H    PICTURE X.
           02  AMT4O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  PCODE5C    PICTURE X.
           02  PCODE5H    PICTURE X.
           02  PCODE5O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY5C    PICTURE X.
           02  QTY5H    PICTURE X.
           02  QTY5O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC5C    PICTURE X.
           02  DESC5H    PICTURE X.
           02  DESC5O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST5C    PICTURE X.
           02  LIST5H    PICTURE X.
           02  LIST5O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET5C    PICTURE X.
           02  NET5H    PICTURE X.
           02  NET5O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT5C    PICTURE X.
           02  AMT5H    PICTURE X.
           02  AMT5O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  PCODE6C    PICTURE X.
           02  PCODE6H    PICTURE X.
           02  PCODE6O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY6C    PICTURE X.
           02  QTY6H    PICTURE X.
           02  QTY6O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC6C    PICTURE X.
           02  DESC6H    PICTURE X.
           02  DESC6O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST6C    PICTURE X.
           02  LIST6H    PICTURE X.
           02  LIST6O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET6C    PICTURE X.
           02  NET6H    PICTURE X.
           02  NET6O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT6C    PICTURE X.
           02  AMT6H    PICTURE X.
           02  AMT6O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  PCODE7C    PICTURE X.
           02  PCODE7H    PICTURE X.
           02  PCODE7O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY7C    PICTURE X.
           02  QTY7H    PICTURE X.
           02  QTY7O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC7C    PICTURE X.
           02  DESC7H    PICTURE X.
           02  DESC7O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST7C    PICTURE X.
           02  LIST7H    PICTURE X.
           02  LIST7O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET7C    PICTURE X.
           02  NET7H    PICTURE X.
           02  NET7O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT7C    PICTURE X.
           02  AMT7H    PICTURE X.
           02  AMT7O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  PCODE8C    PICTURE X.
           02  PCODE8H    PICTURE X.
           02  PCODE8O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY8C    PICTURE X.
           02  QTY8H    PICTURE X.
           02  QTY8O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC8C    PICTURE X.
           02  DESC8H    PICTURE X.
           02  DESC8O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST8C    PICTURE X.
           02  LIST8H    PICTURE X.
           02  LIST8O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET8C    PICTURE X.
           02  NET8H    PICTURE X.
           02  NET8O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT8C    PICTURE X.
           02  AMT8H    PICTURE X.
           02  AMT8O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  PCODE9C    PICTURE X.
           02  PCODE9H    PICTURE X.
           02  PCODE9O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY9C    PICTURE X.
           02  QTY9H    PICTURE X.
           02  QTY9O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC9C    PICTURE X.
           02  DESC9H    PICTURE X.
           02  DESC9O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST9C    PICTURE X.
           02  LIST9H    PICTURE X.
           02  LIST9O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET9C    PICTURE X.
           02  NET9H    PICTURE X.
           02  NET9O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT9C    PICTURE X.
           02  AMT9H    PICTURE X.
           02  AMT9O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  PCODE10C    PICTURE X.
           02  PCODE10H    PICTURE X.
           02  PCODE10O  PIC X(10).
           02  FILLER PICTURE X(3).
           02  QTY10C    PICTURE X.
           02  QTY10H    PICTURE X.
           02  QTY10O  PIC X(5).
           02  FILLER PICTURE X(3).
           02  DESC10C    PICTURE X.
           02  DESC10H    PICTURE X.
           02  DESC10O  PIC X(20).
           02  FILLER PICTURE X(3).
           02  LIST10C    PICTURE X.
           02  LIST10H    PICTURE X.
           02  LIST10O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  NET10C    PICTURE X.
           02  NET10H    PICTURE X.
           02  NET10O PIC ZZZZZZ9.99.
           02  FILLER PICTURE X(3).
           02  AMT10C    PICTURE X.
           02  AMT10H    PICTURE X.
           02  AMT10O PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  TOTALC    PICTURE X.
           02  TOTALH    PICTURE X.
           02  TOTALO PIC Z,ZZZ,ZZ9.99.
           02  FILLER PICTURE X(3).
           02  MSGC    PICTURE X.
           02  MSGH    PICTURE X.
           02  MSGO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  FKEYC    PICTURE X.
           02  FKEYH    PICTURE X.
           02  FKEYO  PIC X(40).
           02  FILLER PICTURE X(3).
           02  DUMMYC    PICTURE X.
           02  DUMMYH    PICTURE X.
           02  DUMMYO  PIC X(1).