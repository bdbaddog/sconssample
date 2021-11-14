      *   Micro Focus BMS Screen Painter (ver MFBM 2.0.11)
      *   MapSet Name   MENSET1
      *   Date Created  04/11/2001
      *   Time Created  12:46:56

      *  Input Data For Map MENMAP1
         01 MENMAP1I.
            03 FILLER                         PIC X(12).
            03 TRANIDL                        PIC S9(4) COMP.
            03 TRANIDF                        PIC X.
            03 FILLER REDEFINES TRANIDF.
               05 TRANIDA                        PIC X.
            03 FILLER                         PIC X(2).
            03 TRANIDI                        PIC X(4).
            03 ACTIONL                        PIC S9(4) COMP.
            03 ACTIONF                        PIC X.
            03 FILLER REDEFINES ACTIONF.
               05 ACTIONA                        PIC X.
            03 FILLER                         PIC X(2).
            03 ACTIONI                        PIC X(1).
            03 MESSAGEL                       PIC S9(4) COMP.
            03 MESSAGEF                       PIC X.
            03 FILLER REDEFINES MESSAGEF.
               05 MESSAGEA                       PIC X.
            03 FILLER                         PIC X(2).
            03 MESSAGEI                       PIC X(79).
            03 DUMMYL                         PIC S9(4) COMP.
            03 DUMMYF                         PIC X.
            03 FILLER REDEFINES DUMMYF.
               05 DUMMYA                         PIC X.
            03 FILLER                         PIC X(2).
            03 DUMMYI                         PIC X(1).

      *  Output Data For Map MENMAP1
         01 MENMAP1O REDEFINES MENMAP1I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TRANIDC                        PIC X.
            03 TRANIDH                        PIC X.
            03 TRANIDO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 ACTIONC                        PIC X.
            03 ACTIONH                        PIC X.
            03 ACTIONO                        PIC X(1).
            03 FILLER                         PIC X(3).
            03 MESSAGEC                       PIC X.
            03 MESSAGEH                       PIC X.
            03 MESSAGEO                       PIC X(79).
            03 FILLER                         PIC X(3).
            03 DUMMYC                         PIC X.
            03 DUMMYH                         PIC X.
            03 DUMMYO                         PIC X(1).

