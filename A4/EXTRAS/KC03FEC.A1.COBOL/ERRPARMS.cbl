      *
      ******************************************************************
      *
      * VARIABLES FOR USE WITH GENERAL ERROR MSG HANDLING (SINGLE LINE)
      *
      ******************************************************************
      *
       01 WS-GM-GENERAL-ERROR-MSG.
           05  WS-GM-RESP                     PIC S9(8) COMP.
           05  WS-GM-RESP2                    PIC S9(8) COMP.
           05  WS-GM-TRNID                    PIC X(4).
           05  WS-GM-RSRCE                    PIC X(8).
           05  WS-GM-MSG                      PIC X(68).
      *
      ******************************************************************
      *
      * VARIABLES FOR USE WITH ERROR MSG HANDLING (EIGHT LINES)
      *
      ******************************************************************
      *
       01 WS-EM-ERROR-MESSAGE.
           05 WS-EM-ERROR-LINE-1.
      *                   ----+----1----+----2----+----3----+----4
               10 FILLER                      PIC X(40)
                   VALUE 'ERROR:Review details and correct problem'.
               10 FILLER                      PIC X(39)
                   VALUE ' in program or in the resource used.   '.
      *                   ----+----1----+----2----+----3----+----
           05 WS-EM-ERROR-LINE-2              PIC X(79) VALUE SPACE.
           05 WS-EM-ERROR-LINE-3.
               10 FILLER                      PIC X(11)
                   VALUE 'EIBRESP  = '.
               10 WS-EM-RESP                  PIC Z(08)9.
               10 FILLER                      PIC X(59) VALUE SPACE.
           05 WS-EM-ERROR-LINE-4.
               10 FILLER                      PIC X(11)
                   VALUE 'EIBRESP2 = '.
               10 WS-EM-RESP2                 PIC Z(08)9.
               10 FILLER                      PIC X(59) VALUE SPACE.
           05 WS-EM-ERROR-LINE-5.
               10 FILLER                      PIC X(11)
                   VALUE 'EIBTRNID = '.
               10 WS-EM-TRNID                 PIC X(04).
               10 FILLER                      PIC X(64) VALUE SPACE.
           05 WS-EM-ERROR-LINE-6.
               10 FILLER                      PIC X(11)
                   VALUE 'EIBRSRCE = '.
               10 WS-EM-RSRCE                 PIC X(08).
               10 FILLER                      PIC X(60) VALUE SPACE.
           05 WS-EM-ERROR-LINE-7.
               10 FILLER                      PIC X(11)
                   VALUE 'MESSAGE  = '.
               10 WS-EM-MSG                   PIC X(68).
           05 WS-EM-ERROR-LINE-8              PIC X(79) VALUE SPACE.
      *
      ******************************************************************
      *
      * VARIABLES FOR USE WITH HANDLE ABEND LOGIC
      *
      ******************************************************************
      *
       01 WS-HA-HANDLE-ABEND-MSG.
           05 WS-HA-MSG-T1                    PIC X(79) VALUE SPACES.
           05 WS-HA-MSG-T2                    PIC X(79) VALUE SPACES.
           05 WS-HA-MSG-T3                    PIC X(79) VALUE SPACES.
           05 WS-HA-MSG-T4                    PIC X(79) VALUE SPACES.
           05 WS-HA-MSG-T5                    PIC X(79) VALUE SPACES.
           05 WS-HA-MSG-T6                    PIC X(79) VALUE SPACES.
           05 WS-HA-MSG-T7                    PIC X(79) VALUE SPACES.
           05 WS-HA-MSG-T8                    PIC X(79) VALUE SPACES.
      *
       01 WS-HANDLE-ABEND-MSGS.
           05 WS-HA-UNEXPECTED-ABEND          PIC X(20)
               VALUE 'ABEND UNEXPECTED    '.
      *               ----+----0----+----1
           05 WS-HA-UNHANDLED-ABEND           PIC X(20)
               VALUE 'ABEND UNHANDLED     '.
      *               ----+----0----+----1
       01 WS-HA-EXEC-TEXT.
           05 WS-HA-EXEC-TEXT-T1              PIC X(79) VALUE SPACES.
           05 WS-HA-EXEC-TEXT-T2              PIC X(79) VALUE SPACES.
           05 WS-HA-EXEC-TEXT-T3              PIC X(79) VALUE SPACES.
           05 WS-HA-EXEC-TEXT-T4              PIC X(79) VALUE SPACES.
           05 WS-HA-EXEC-TEXT-T5              PIC X(79) VALUE SPACES.
           05 WS-HA-EXEC-TEXT-T6              PIC X(79) VALUE SPACES.
           05 WS-HA-EXEC-TEXT-T7              PIC X(79) VALUE SPACES.
           05 WS-HA-EXEC-TEXT-T8              PIC X(79) VALUE SPACES.
      *
       01 WS-HA-ERR-MSG-NBRS.
           05 WS-HA-ERR-MSG-FILENOTFOUND      PIC 9(3)  VALUE 12.
           05 WS-HA-ERR-MSG-PGMIDERR          PIC 9(3)  VALUE 27.
           05 WS-HA-ERR-MSG-TRANSIDERR        PIC 9(3)  VALUE 28.
           05 WS-HA-ERR-MSG-MAPFAIL           PIC 9(3)  VALUE 36.
      *
      ******************************************************************
      *
      * VARIABLES FOR USE WITH RESPONSE TEXT TABLE LOOKUP LOGIC
      * WRITTEN BY - GREGORY OAKES - 2021-APR-15
      *
      ******************************************************************
      * TRANSLATE EXEC CICS COMMANDS FROM NUMERIC VALUES TO X(15) TEXT
      * IN CICS PROGRAMS AND DISPLAY ERROR MESSAGE TO DECODE CODE ISSUES
      * OR HELP DEBUG ABEND ISSUES.
      *
      ******************************************************************
      *                                               123456789012345
       01 WS-RT-MSG-TABLE-DATA.
           05 WS-RT-001-ERROR        PIC X(15) VALUE "ERROR          ".
           05 WS-RT-002-RDATT        PIC X(15) VALUE "RDATT          ".
           05 WS-RT-003-WRBRK        PIC X(15) VALUE "WRBRK          ".
           05 WS-RT-004-EOF          PIC X(15) VALUE "EOF            ".
           05 WS-RT-005-EODS         PIC X(15) VALUE "EODS           ".
           05 WS-RT-006-EOC          PIC X(15) VALUE "EOC            ".
           05 WS-RT-007-INBFMH       PIC X(15) VALUE "INBFMH         ".
           05 WS-RT-008-ENDINPT      PIC X(15) VALUE "ENDINPT        ".
           05 WS-RT-009-NONVAL       PIC X(15) VALUE "NONVAL         ".
           05 WS-RT-010-NOSTART      PIC X(15) VALUE "NOSTART        ".
           05 WS-RT-011-TERMIDERR    PIC X(15) VALUE "TERMIDERR      ".
           05 WS-RT-012-FILENOTFOUND PIC X(15) VALUE "FILENOTFOUND   ".
           05 WS-RT-013-NOTFND       PIC X(15) VALUE "NOTFND         ".
           05 WS-RT-014-DUPREC       PIC X(15) VALUE "DUPREC         ".
           05 WS-RT-015-DUPKEY       PIC X(15) VALUE "DUPKEY         ".
           05 WS-RT-016-INVREQ       PIC X(15) VALUE "INVREQ         ".
           05 WS-RT-017-IOERR        PIC X(15) VALUE "IOERR          ".
           05 WS-RT-018-NOSPACE      PIC X(15) VALUE "NOSPACE        ".
           05 WS-RT-019-NOTOPEN      PIC X(15) VALUE "NOTOPEN        ".
           05 WS-RT-020-ENDFILE      PIC X(15) VALUE "ENDFILE        ".
           05 WS-RT-021-ILLOGIC      PIC X(15) VALUE "ILLOGIC        ".
           05 WS-RT-022-LENGERR      PIC X(15) VALUE "LENGERR        ".
           05 WS-RT-023-QZERO        PIC X(15) VALUE "QZERO          ".
           05 WS-RT-024-SIGNAL       PIC X(15) VALUE "SIGNAL         ".
           05 WS-RT-025-QBUSY        PIC X(15) VALUE "QBUSY          ".
           05 WS-RT-026-ITEMERR      PIC X(15) VALUE "ITEMERR        ".
           05 WS-RT-027-PGMIDERR     PIC X(15) VALUE "PGMIDERR       ".
           05 WS-RT-028-TRANSIDERR   PIC X(15) VALUE "TRANSIDERR     ".
           05 WS-RT-029-ENDDATA      PIC X(15) VALUE "ENDDATA        ".
           05 WS-RT-030-INVTSREQ     PIC X(15) VALUE "INVTSREQ       ".
           05 WS-RT-031-EXPIRED      PIC X(15) VALUE "EXPIRED        ".
           05 WS-RT-032-RETPAGE      PIC X(15) VALUE "RETPAGE        ".
           05 WS-RT-033-RTEFAIL      PIC X(15) VALUE "RTEFAIL        ".
           05 WS-RT-034-RTESOME      PIC X(15) VALUE "RTESOME        ".
           05 WS-RT-035-TSIOERR      PIC X(15) VALUE "TSIOERR        ".
           05 WS-RT-036-MAPFAIL      PIC X(15) VALUE "MAPFAIL        ".
           05 WS-RT-037-INVERRTERM   PIC X(15) VALUE "INVERRTERM     ".
           05 WS-RT-038-INVMPSZ      PIC X(15) VALUE "INVMPSZ        ".
           05 WS-RT-039-IGREQID      PIC X(15) VALUE "IGREQID        ".
           05 WS-RT-040-OVERFLOW     PIC X(15) VALUE "OVERFLOW       ".
           05 WS-RT-041-INVLDC       PIC X(15) VALUE "INVLDC         ".
           05 WS-RT-042-NOSTG        PIC X(15) VALUE "NOSTG          ".
           05 WS-RT-043-JIDERR       PIC X(15) VALUE "JIDERR         ".
           05 WS-RT-044-QIDERR       PIC X(15) VALUE "QIDERR         ".
           05 WS-RT-045-NOJBUFSP     PIC X(15) VALUE "NOJBUFSP       ".
           05 WS-RT-046-DSSTAT       PIC X(15) VALUE "DSSTAT         ".
           05 WS-RT-047-SELNERR      PIC X(15) VALUE "SELNERR        ".
           05 WS-RT-048-FUNCERR      PIC X(15) VALUE "FUNCERR        ".
           05 WS-RT-049-UNEXPIN      PIC X(15) VALUE "UNEXPIN        ".
           05 WS-RT-050-NOPASSBKRD   PIC X(15) VALUE "NOPASSBKRD     ".
           05 WS-RT-051-NOPASSBKWR   PIC X(15) VALUE "NOPASSBKWR     ".
           05 WS-RT-052-SEGIDERR     PIC X(15) VALUE "SEGIDERR       ".
           05 WS-RT-053-SYSIDERR     PIC X(15) VALUE "SYSIDERR       ".
           05 WS-RT-054-ISCINVREQ    PIC X(15) VALUE "ISCINVREQ      ".
           05 WS-RT-055-ENQBUSY      PIC X(15) VALUE "ENQBUSY        ".
           05 WS-RT-056-ENVDEFERR    PIC X(15) VALUE "ENVDEFERR      ".
           05 WS-RT-057-IGREQCD      PIC X(15) VALUE "IGREQCD        ".
           05 WS-RT-058-SESSIONERR   PIC X(15) VALUE "SESSIONERR     ".
           05 WS-RT-059-SYSBUSY      PIC X(15) VALUE "SYSBUSY        ".
           05 WS-RT-060-SESSBUSY     PIC X(15) VALUE "SESSBUSY       ".
           05 WS-RT-061-NOTALLOC     PIC X(15) VALUE "NOTALLOC       ".
           05 WS-RT-062-CBIDERR      PIC X(15) VALUE "CBIDERR        ".
           05 WS-RT-063-INVEXITREQ   PIC X(15) VALUE "INVEXITREQ     ".
           05 WS-RT-064-INVPARTNSET  PIC X(15) VALUE "INVPARTNSET    ".
           05 WS-RT-065-INVPARTN     PIC X(15) VALUE "INVPARTN       ".
           05 WS-RT-066-PARTNFAIL    PIC X(15) VALUE "PARTNFAIL      ".
           05 WS-RT-067-NOTUSED      PIC X(15) VALUE "NOTUSED        ".
           05 WS-RT-068-NOTUSED      PIC X(15) VALUE "NOTUSED        ".
           05 WS-RT-069-USERIDERR    PIC X(15) VALUE "USERIDERR      ".
           05 WS-RT-070-NOTAUTH      PIC X(15) VALUE "NOTAUTH        ".
           05 WS-RT-071-VOLIDERR     PIC X(15) VALUE "VOLIDERR       ".
           05 WS-RT-072-SUPPRESSED   PIC X(15) VALUE "SUPPRESSED     ".
           05 WS-RT-073-NOTUSED      PIC X(15) VALUE "NOTUSED        ".
           05 WS-RT-074-NOTUSED      PIC X(15) VALUE "NOTUSED        ".
           05 WS-RT-075-RESIDERR     PIC X(15) VALUE "RESIDERR       ".
           05 WS-RT-076-NOTUSED      PIC X(15) VALUE "NOTUSED        ".
           05 WS-RT-077-NOTUSED      PIC X(15) VALUE "NOTUSED        ".
           05 WS-RT-078-NOTUSED      PIC X(15) VALUE "NOTUSED        ".
           05 WS-RT-079-NOTUSED      PIC X(15) VALUE "NOTUSED        ".
           05 WS-RT-080-NOSPOOL      PIC X(15) VALUE "NOSPOOL        ".
           05 WS-RT-081-TERMERR      PIC X(15) VALUE "TERMERR        ".
           05 WS-RT-082-ROLLEDBACK   PIC X(15) VALUE "ROLLEDBACK     ".
           05 WS-RT-083-END          PIC X(15) VALUE "END            ".
           05 WS-RT-084-DISABLED     PIC X(15) VALUE "DISABLED       ".
           05 WS-RT-085-ALLOCERR     PIC X(15) VALUE "ALLOCERR       ".
           05 WS-RT-086-STRELERR     PIC X(15) VALUE "STRELERR       ".
           05 WS-RT-087-OPENERR      PIC X(15) VALUE "OPENERR        ".
           05 WS-RT-088-SPOLBUSY     PIC X(15) VALUE "SPOLBUSY       ".
           05 WS-RT-089-SPOLERR      PIC X(15) VALUE "SPOLERR        ".
           05 WS-RT-090-NODEIDERR    PIC X(15) VALUE "NODEIDERR      ".
           05 WS-RT-091-TASKIDERR    PIC X(15) VALUE "TASKIDERR      ".
           05 WS-RT-092-TCIDERR      PIC X(15) VALUE "TCIDERR        ".
           05 WS-RT-093-DSNNOTFOUND  PIC X(15) VALUE "DSNNOTFOUND    ".
           05 WS-RT-094-LOADING      PIC X(15) VALUE "LOADING        ".
           05 WS-RT-095-MODELIDERR   PIC X(15) VALUE "MODELIDERR     ".
           05 WS-RT-096-OUTDESCRERR  PIC X(15) VALUE "OUTDESCRERR    ".
           05 WS-RT-097-PARTNERIDERR PIC X(15) VALUE "PARTNERIDERR   ".
           05 WS-RT-098-PROFILEIDERR PIC X(15) VALUE "PROFILEIDERR   ".
           05 WS-RT-099-NETNAMEIDERR PIC X(15) VALUE "NETNAMEIDERR   ".
           05 WS-RT-100-LOCKED       PIC X(15) VALUE "LOCKED         ".
           05 WS-RT-101-RECORDBUSY   PIC X(15) VALUE "RECORDBUSY     ".
           05 WS-RT-102-UOWNOTFOUND  PIC X(15) VALUE "UOWNOTFOUND    ".
           05 WS-RT-103-UOWLNOTFOUND PIC X(15) VALUE "UOWLNOTFOUND   ".
           05 WS-RT-104-LINKABEND    PIC X(15) VALUE "LINKABEND      ".
           05 WS-RT-105-CHANGED      PIC X(15) VALUE "CHANGED        ".
           05 WS-RT-106-PROCESSBUSY  PIC X(15) VALUE "PROCESSBUSY    ".
           05 WS-RT-107-ACTIVITYBUSY PIC X(15) VALUE "ACTIVITYBUSY   ".
           05 WS-RT-108-PROCESSERR   PIC X(15) VALUE "PROCESSERR     ".
           05 WS-RT-109-ACTIVITYERR  PIC X(15) VALUE "ACTIVITYERR    ".
           05 WS-RT-110-CONTAINERERR PIC X(15) VALUE "CONTAINERERR   ".
           05 WS-RT-111-EVENTERR     PIC X(15) VALUE "EVENTERR       ".
           05 WS-RT-112-TOKENERR     PIC X(15) VALUE "TOKENERR       ".
           05 WS-RT-113-NOTFINISHED  PIC X(15) VALUE "NOTFINISHED    ".
           05 WS-RT-114-POOLERR      PIC X(15) VALUE "POOLERR        ".
           05 WS-RT-115-TIMERERR     PIC X(15) VALUE "TIMERERR       ".
           05 WS-RT-116-SYMBOLERR    PIC X(15) VALUE "SYMBOLERR      ".
           05 WS-RT-117-TEMPLATERR   PIC X(15) VALUE "TEMPLATERR     ".
           05 WS-RT-118-NOTSUPERUSER PIC X(15) VALUE "NOTSUPERUSER   ".
           05 WS-RT-119-CSDERR       PIC X(15) VALUE "CSDERR         ".
           05 WS-RT-120-DUPRES       PIC X(15) VALUE "DUPRES         ".
           05 WS-RT-121-RESUNAVAIL   PIC X(15) VALUE "RESUNAVAIL     ".
           05 WS-RT-122-CHANNELERR   PIC X(15) VALUE "CHANNELERR     ".
           05 WS-RT-123-CCSIDERR     PIC X(15) VALUE "CCSIDERR       ".
           05 WS-RT-124-TIMEDOUT     PIC X(15) VALUE "TIMEDOUT       ".
           05 WS-RT-125-CODEPAGEERR  PIC X(15) VALUE "CODEPAGEERR    ".
           05 WS-RT-126-INCOMPLETE   PIC X(15) VALUE "INCOMPLETE     ".
           05 WS-RT-127-APPNOTFOUND  PIC X(15) VALUE "APPNOTFOUND    ".
      *                                               123456789012345
      *
       01 WS-RT-MSG-TABLE-DATA-R REDEFINES WS-RT-MSG-TABLE-DATA.
           05 WS-RT-MSG-NBR-TEXT     PIC X(15) OCCURS 127 TIMES.
      *
       01 WS-RT-RESP-NBR             PIC S9(8) VALUE 0.
           88 WS-RT-RESP-NBR-VALID-88 VALUE 1 THRU 127.
      *
       01 WS-RT-RESP-NBR-INVALID-MSG PIC X(15) VALUE "INVALIDNBR".
      *
       01 WS-RT-MSG.
           05 WS-RT-MSG-HDR          PIC X(15) VALUE SPACES.
           05 WS-RT-MSG-RESP-NBR     PIC S9(8) VALUE 0.
           05 WS-RT-MSG-T1           PIC X(15) VALUE SPACES.
           05 WS-RT-MSG-T2           PIC X(30) VALUE SPACES.
      *
       01 WS-RT-MSG-HEADER           PIC X(30) VALUE SPACES.
      *
       01 WS-RT-MSG-NORMAL           PIC X(30) VALUE SPACES.
      *
       01 WS-RT-MSG-OTHER            PIC X(30) VALUE SPACES.
      *
      ******************************************************************
      *
