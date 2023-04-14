       01  MENMFECI.
           02  FILLER PIC X(12).
           02  TRANIDL    COMP  PIC  S9(4).
           02  TRANIDF    PICTURE X.
           02  FILLER REDEFINES TRANIDF.
             03 TRANIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  TRANIDI  PIC X(4).
           02  ACTIONL    COMP  PIC  S9(4).
           02  ACTIONF    PICTURE X.
           02  FILLER REDEFINES ACTIONF.
             03 ACTIONA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  ACTIONI  PIC X(1).
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
       01  MENMFECO REDEFINES MENMFECI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  TRANIDC    PICTURE X.
           02  TRANIDH    PICTURE X.
           02  TRANIDO  PIC X(4).
           02  FILLER PICTURE X(3).
           02  ACTIONC    PICTURE X.
           02  ACTIONH    PICTURE X.
           02  ACTIONO  PIC X(1).
           02  FILLER PICTURE X(3).
           02  MESSAGEC    PICTURE X.
           02  MESSAGEH    PICTURE X.
           02  MESSAGEO  PIC X(79).
           02  FILLER PICTURE X(3).
           02  DUMMYC    PICTURE X.
           02  DUMMYH    PICTURE X.
           02  DUMMYO  PIC X(1).