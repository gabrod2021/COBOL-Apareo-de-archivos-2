       01 WS-SALIDA-APAREO.

          05 WS-SAL-APA-SEPARADOR           PIC X(86) VALUES ALL "-".

          05 WS-SAL-APA-TITULOS.
             10 FILLER                      PIC X(11)
                                                VALUE 'ID-EMPLEADO'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(25) VALUE 'NOMBRE'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(25) VALUE 'APELLIDO'.
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 FILLER                      PIC X(16) VALUE 'IMPORTE'.

          05 WS-SAL-APA-DETALLE.
             10 WS-SAL-APA-ID-EMPLEADO      PIC 9(08).
             10 FILLER                      PIC X(06) VALUE '    | '.
             10 WS-SAL-APA-NOMBRE           PIC X(25).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-SAL-APA-APELLIDO         PIC X(25).
             10 FILLER                      PIC X(03) VALUE ' | '.
             10 WS-SAL-APA-IMPORTE          PIC Z.ZZZ.ZZZ.ZZ9,99.
