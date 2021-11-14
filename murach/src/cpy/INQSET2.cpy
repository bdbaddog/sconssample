      *   Micro Focus BMS Screen Painter (ver MFBM 2.0.11)
      *   MapSet Name   INQSET2
      *   Date Created  04/16/2001
      *   Time Created  15:17:42

      *  Input Data For Map INQMAP2
         01 INQMAP2I.
            03 FILLER                         PIC X(12).
            03 TRANIDL                        PIC S9(4) COMP.
            03 TRANIDF                        PIC X.
            03 FILLER REDEFINES TRANIDF.
               05 TRANIDA                        PIC X.
            03 FILLER                         PIC X(2).
            03 TRANIDI                        PIC X(4).
            03 CUSTNOL                        PIC S9(4) COMP.
            03 CUSTNOF                        PIC X.
            03 FILLER REDEFINES CUSTNOF.
               05 CUSTNOA                        PIC X.
            03 FILLER                         PIC X(2).
            03 CUSTNOI                        PIC X(6).
            03 LNAMEL                         PIC S9(4) COMP.
            03 LNAMEF                         PIC X.
            03 FILLER REDEFINES LNAMEF.
               05 LNAMEA                         PIC X.
            03 FILLER                         PIC X(2).
            03 LNAMEI                         PIC X(30).
            03 FNAMEL                         PIC S9(4) COMP.
            03 FNAMEF                         PIC X.
            03 FILLER REDEFINES FNAMEF.
               05 FNAMEA                         PIC X.
            03 FILLER                         PIC X(2).
            03 FNAMEI                         PIC X(20).
            03 ADDRL                          PIC S9(4) COMP.
            03 ADDRF                          PIC X.
            03 FILLER REDEFINES ADDRF.
               05 ADDRA                          PIC X.
            03 FILLER                         PIC X(2).
            03 ADDRI                          PIC X(30).
            03 CITYL                          PIC S9(4) COMP.
            03 CITYF                          PIC X.
            03 FILLER REDEFINES CITYF.
               05 CITYA                          PIC X.
            03 FILLER                         PIC X(2).
            03 CITYI                          PIC X(20).
            03 STATEL                         PIC S9(4) COMP.
            03 STATEF                         PIC X.
            03 FILLER REDEFINES STATEF.
               05 STATEA                         PIC X.
            03 FILLER                         PIC X(2).
            03 STATEI                         PIC X(2).
            03 ZIPCODEL                       PIC S9(4) COMP.
            03 ZIPCODEF                       PIC X.
            03 FILLER REDEFINES ZIPCODEF.
               05 ZIPCODEA                       PIC X.
            03 FILLER                         PIC X(2).
            03 ZIPCODEI                       PIC X(10).
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

      *  Output Data For Map INQMAP2
         01 INQMAP2O REDEFINES INQMAP2I.
            03 FILLER                         PIC X(12).
            03 FILLER                         PIC X(3).
            03 TRANIDC                        PIC X.
            03 TRANIDH                        PIC X.
            03 TRANIDO                        PIC X(4).
            03 FILLER                         PIC X(3).
            03 CUSTNOC                        PIC X.
            03 CUSTNOH                        PIC X.
            03 CUSTNOO                        PIC X(6).
            03 FILLER                         PIC X(3).
            03 LNAMEC                         PIC X.
            03 LNAMEH                         PIC X.
            03 LNAMEO                         PIC X(30).
            03 FILLER                         PIC X(3).
            03 FNAMEC                         PIC X.
            03 FNAMEH                         PIC X.
            03 FNAMEO                         PIC X(20).
            03 FILLER                         PIC X(3).
            03 ADDRC                          PIC X.
            03 ADDRH                          PIC X.
            03 ADDRO                          PIC X(30).
            03 FILLER                         PIC X(3).
            03 CITYC                          PIC X.
            03 CITYH                          PIC X.
            03 CITYO                          PIC X(20).
            03 FILLER                         PIC X(3).
            03 STATEC                         PIC X.
            03 STATEH                         PIC X.
            03 STATEO                         PIC X(2).
            03 FILLER                         PIC X(3).
            03 ZIPCODEC                       PIC X.
            03 ZIPCODEH                       PIC X.
            03 ZIPCODEO                       PIC X(10).
            03 FILLER                         PIC X(3).
            03 MESSAGEC                       PIC X.
            03 MESSAGEH                       PIC X.
            03 MESSAGEO                       PIC X(79).
            03 FILLER                         PIC X(3).
            03 DUMMYC                         PIC X.
            03 DUMMYH                         PIC X.
            03 DUMMYO                         PIC X(1).

