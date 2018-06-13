//R72182UT JOB (KBS),'UTILERIAS',
//         CLASS=M,NOTIFY=&SYSUID,REGION=0M,
//         MSGCLASS=X,MSGLEVEL=(1,1)
//*  PROD
//*       JCLLIB  ORDER=PNDV.NOPUT.PROD.B.RCQ.B.JCL
//*       INCLUDE MEMBER=LIBRCQ
//*       SET PARMS=PNDV.NOPUT.PROD.B.RCQ.B.PARM
//*  OAT
//*       JCLLIB  ORDER=LNDV.NOPUT.OPERTEST.B.RCQ.B.JCL.C
//*       INCLUDE MEMBER=LIBRCQ
//*       SET PARMS=LNDV.NOPUT.OPERTEST.B.RCQ.B.PARM.C
//*  DESA
//*        JCLLIB  ORDER=DRCR.PJCL.P22DS
//*        INCLUDE MEMBER=LIBRCQ
//*************************************************************
//*    BORRA ARCHIVOS DE TRABAJO
//*************************************************************
//BORRA EXEC PGM=IDCAMS
//SYSDBOUT DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSPRINT DD   SYSOUT=*
//SYSABEND DD   DUMMY
//SYSIN    DD *
  DELETE (PKRN.SSUNIVER.A1.P22DS)   NONVSAM SCRATCH
  DELETE (PRCQ.SCV.CIS.P22DS)  SCRATCH                          * vsam
  IF MAXCC <= 8 THEN SET MAXCC = 0
//*************************************************************
//*      CREACION DE DATA SETS
//*************************************************************
//RCRE3403  EXEC PGM=IEFBR14
//RESTNCAR   DD DSN=PRCQ.SNV.PYMES.SNCAR.P22DS,
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(LRECL=48,BLKSIZE=27984,RECFM=FB,BUFNO=30),
//            SPACE=(TRK,(850,85),RLSE)
//ERRMEMOS   DD DSN=PRCQ.SNV.MEMOS.SCARD.ERRORES.P22DS,
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(LRECL=80,BLKSIZE=27920,RECFM=FB,BUFNO=30),
//            SPACE=(TRK,(500,500),RLSE)
//*
//*************************************************************
//*            CREACION DE VSAM
//*************************************************************
//RCRD3402 EXEC PGM=IDCAMS
//SYSOUT   DD   SYSOUT=*
//SYSPRINT DD   SYSOUT=*
//SYSABEND DD   DUMMY
//SYSIN    DD   *
  DEFINE CLUSTER                                      -
         (NAME(PRCR.SCV.CAT.SEPMX.P22DS)              -
          RECORDSIZE(136 136)                         -
          KEYS(5 0)                                   -
          INDEXED                                     -
          SPEED                                       -
          SHAREOPTIONS(2 3)                           -
         )                                            -
         DATA                                         -
         (NAME(PRCR.SCV.CAT.SEPMX.P22DS.DATA)         -
          TRK(50 20)                                  -
         )                                            -
         INDEX                                        -
         (NAME(PRCR.SCV.CAT.SEPMX.P22DS.INDEX))
//*************************************************************
//*            CREACION DE IAM
//*************************************************************
//RCQD3101 EXEC PGM=IDCAMS,COND=(4,LT)
//SYSOUT   DD   SYSOUT=*
//SYSPRINT DD   SYSOUT=*
//SYSABEND DD   DUMMY
//SYSIN    DD   *
  DEFINE CLUSTER                                        -
           (NAME(QRCQ.SCV.CIS.P22DS)                    -
            INDEXED REUSE SPEED                         -
            TRK(50 20) SHR(2)                           -
            OWNER ( $IAM )                              -
            VOL(VOLSER VOLSER VOLSER)                   -
           )                                            -
           DATA                                         -
           (NAME(QRCQ.SCV.CIS.P22DS.DATA)               -
            KEYS(15 0)                                  -
            RECORDSIZE(15 15)                           -
           )                                            -
           INDEX                                        -
           (NAME(QRCQ.SCV.CIS.P22DS.INDEX)              -
           NOIMBED NOREPLICATE)
//*************************************************************
//*       CREACION NUEVO GENERACIONAL  (GDG)
//*************************************************************
//STEP001  EXEC PGM=IDCAMS
//SYSDBOUT DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSPRINT DD   SYSOUT=*
//SYSABEND DD   DUMMY
//SYSIN    DD   *
//ARCH1    DD  DSN=QRCQ.S001.RCQ10601.KRUNIV.P22DS(+1),
//             DISP=(NEW,CATLG,CATLG),
//             DCB=(LRECL=1328,BLKSIZE=27888,RECFM=FB),
//             SPACE=(TRK,(100,10),RLSE)
//*************************************************************
//*         COPIA UN ARCHIVO A OTRO                           *
//*************************************************************
//RCRD0305 EXEC PGM=IDCAMS,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//ENTRADA  DD DSN=PRCR.SNV.CAT.SEPMX.P22DS,DISP=OLD
//SALIDA   DD DSN=PRCR.SCV.CAT.SEPMX.P22DS,DISP=OLD
//SYSIN    DD *
  REPRO INFILE(ENTRADA) OUTFILE(SALIDA) REPLACE
//***********************************************************
//*                       LIMPIA DATASET
//***********************************************************
//STEP010  EXEC PGM=IDCAMS,COND=(0,LT)
//SYSDBOUT DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSPRINT DD   SYSOUT=*
//SYSABEND DD   DUMMY
//ENTRADA  DD   DUMMY
//SALIDA   DD  DSN=QRCR.S001.RCR17003.DIREC.ANT.P22DS,DISP=SHR
//SYSIN    DD   *
   REPRO INFILE(ENTRADA) OUTFILE(SALIDA) REUSE
//*
//*************************************************************
//*           VERIFICACION DE VSAM
//*************************************************************
//RCR91A02  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN DD  *
        VERIFY DS(PRCR.SCV.CAT.SEPMX.P22DS)
        IF MAXCC = 12 THEN SET MAXCC = 0
//*
//**********************************************************************
//*  DESCRIPCION: RENOMBRADO DE UNIVERSAL ORIGINAL A BKP               *
//**********************************************************************
//KR975E01    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
   ALTER PKRN.SSUNIVER.A1.P22DS -
        NEWNAME(PKRN.SSUNIVER.BKP1.P22DS)
//*********************************************************************
//*   UNE 2 ARCHIVOS DIFERENTES EN UN FORMATO NUEVO DE SALIDA
//*********************************************************************
//RCQ08LG2 EXEC PGM=SORT
//SORTJNF1 DD DSN=PRCQ.SMIS.D82902.LOGTELE.P22DS,DISP=SHR
//SORTJNF2 DD DSN=&&COUNT,DISP=SHR
//SORTOUT  DD DSN=&&LOGTELE,UNIT=SYSDA,SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=208,BLKSIZE=27872),DSORG=PS,
//            DISP=(MOD,PASS)
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
  JOINKEYS FILE=F1,FIELDS=(1,3,A)
  JOINKEYS FILE=F2,FIELDS=(2,3,A)
  REFORMAT FIELDS=(F1:1,101,F2:5,11,F1:113,96)
  SORT FIELDS=COPY
//**********************************************************************
//*  COPIAR UN ARCHIVO A OTRO, SALTANDO x CANTIDAD DE REGISTROS
//**********************************************************************
//SORT001  EXEC PGM=SORT,PARM=('DYNALLOC=(SYSALLDA,32)')
//SORTIN   DD DISP=SHR,DSN=DTBS.SSINV.FDBKUIP.LDD.P22DS
//SORTOUT  DD DSN=DTBS.SSINV.FDBKUIP.LDD2.P22DS,DISP=SHR
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SORT FIELDS=COPY
  OPTION SKIPREC=481772
//*-----------------------------------------------------------*
//*       ELIMINA DUPLICADOS, DEJA SOLO 1
//*-----------------------------------------------------------*
//SPTEP001 EXEC PGM=SORT,COND=(4,LT)
//SYSPRINT DD SYSOUT=&SYSOUT
//SYSOUT   DD SYSOUT=&SYSOUT
//SORTIN   DD DSN=&NVSM..SNVCIS.RCR05202.EXTMPREV.P&PART..P22DS,
//         DISP=SHR
//SORTOUT  DD DSN=&NVSM..SNVCIS.RCR05201.EXTRMAIL.P&PART..P22DS,
//         DISP=(NEW,CATLG,CATLG),SPACE=&SPACE01
//SYSIN    DD   *
   SORT FIELDS=(1,15,CH,A)
   SUM  FIELDS=NONE
//*
//*-----------------------------------------------------------*
//* REEMPLAZAR LAS ESTRATEGIAS 3->1 Y 4->2
//*-----------------------------------------------------------*
//STEP002   EXEC PGM=SORT,COND=(4,LT)
//SORTIN    DD DSN=&NVSM..SNVCIS.RCR34701.ESTRATG&PART..P22DS,
//             DISP=SHR
//SORTOUT   DD DSN=&&ESTRTGIA,
//         DISP=(MOD,PASS),UNIT=SYSDA,
//         DCB=(RECFM=FB,LRECL=70,BLKSIZE=27930),DSORG=PS,
//         SPACE=&SPACE01
//SORTOUT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SORT FIELDS=COPY
  OUTREC IFTHEN=(WHEN=(56,1,CH,EQ,C'3'),
             OVERLAY=(1,55,C'1',57,13)),
         IFTHEN=(WHEN=(56,1,CH,EQ,C'4'),
             OVERLAY=(1,55,C'2',57,13))
//**********************************************************************
//*FORMATEAR UN ARCHIVO DE SALIDA (POSICION DEL FILE ORIGINAL,LONGOTUD)
//**********************************************************************
//SORT001  EXEC PGM=SORT
//SORTIN   DD DSN=QRCQ.SCYB.D94102.BCOCAST.KRONER.P22DS,DISP=SHR
//SORTOUT  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  SORT FIELDS=COPY
  OUTREC FIELDS=(16,8)
//*  CONVERSION DE DATOS
//*OUTREC FIELDS=(inicio_campo1,
//*               longitud_campo,
//*               tipo_dato_inicial,
//*               TO=tipo_dato_de_destino,
//*               LENGTH=longitud_final_del_campo_convertido,
//*               )
//*Los tipos de formato son los siguientes:
//*CH --> Alfanumérico
//*ZD --> Numérico normal
//*BI --> Hexadecimal (campos COMP)
//*PD --> Empaquetado con o sin signo(campos COMP-3)
//**********************************************************************
//*  ORDENAR ARCHIVO POR LA POSICION 16, 8 DIGITOS
//**********************************************************************
//RCQ91706 EXEC PGM=SORT,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=PRCQ.SCYB.D91602.BCOCAST1.KRONER.P22DS,DISP=SHR
//SORTOUT  DD DSN=PRCQ.SCYB.D91602.BCOCAST1.KRONER.P22DS,DISP=SHR
//SORTWK01 DD SPACE=(CYL,(1500,500),RLSE)
//SORTWK02 DD SPACE=(CYL,(1500,500),RLSE)
//SORTWK03 DD SPACE=(CYL,(1500,500),RLSE)
//SORTWK04 DD SPACE=(CYL,(1500,500),RLSE)
//SORTWK05 DD SPACE=(CYL,(1500,500),RLSE)
//SORTWK06 DD SPACE=(CYL,(1500,500),RLSE)
//SYSIN    DD *
   SORT FIELDS=(16,8,A),FORMAT=CH
//**********************************************************************
//*     ORDENAR ARCHIVO POR CIS Y LA NUEVA ESTRATEGIA
//**********************************************************************
//RCQ91706 EXEC PGM=SORT,COND=(4,LT)
//SORTIN   DD DSN=&&ESTRTGIA,DISP=SHR
//SORTOUT  DD DSN=&&ESTRTGIA,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
   SORT FIELDS=(22,15,A,56,1,A),FORMAT=CH
//*********************************************************************
//*  UNIR VARIOS ARCHIVOS Y ORDENARLOS
//*********************************************************************
//RCQ91602 EXEC PGM=SORT,COND=(4,LT)
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=PRCQ.SCUENTAS.PART00.P22DS,DISP=SHR
//         DD DSN=PRCQ.SCUENTAS.PART01.P22DS,DISP=SHR
//         DD DSN=PRCQ.SCUENTAS.PART02.P22DS,DISP=SHR
//         DD DSN=PRCQ.SCUENTAS.PART03.P22DS,DISP=SHR
//SORTOUT  DD DSN=PRCQ.SCUENTAS.ALL.P22DS,
//            DISP=(NEW,CATLG,DELETE),
//            DCB=(RECFM=FB,LRECL=1729,BLKSIZE=27664),DSORG=PS,
//            SPACE=(TRK,(1500,500),RLSE)
//SYSIN    DD *
  SORT FIELDS=(16,8,A),FORMAT=CH
//*************************************************************
//*         DIVIDE EN 10 ARCHIVOS
//*************************************************************
//RCQE0011 EXEC PGM=SORT,COND=(4,LT)
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//SORTIN    DD DSN=PRCQ.SNV.DELQMST.T22DS,DISP=SHR
//OUT1      DD DSN=PRCQ.SNV.DELQMST.P01.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//OUT2      DD DSN=PRCQ.SNV.DELQMST.P02.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//OUT3      DD DSN=PRCQ.SNV.DELQMST.P03.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//OUT4      DD DSN=PRCQ.SNV.DELQMST.P04.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//OUT5      DD DSN=PRCQ.SNV.DELQMST.P05.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//OUT6      DD DSN=PRCQ.SNV.DELQMST.P06.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//OUT7      DD DSN=PRCQ.SNV.DELQMST.P07.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//OUT8      DD DSN=PRCQ.SNV.DELQMST.P08.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//OUT9      DD DSN=PRCQ.SNV.DELQMST.P09.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//OUT0      DD DSN=PRCQ.SNV.DELQMST.P10.T22DS,
//             DISP=(NEW,CATLG,DELETE),
//             DCB=(RECFM=FB,LRECL=219,BLKSIZE=27813,BUFNO=30),
//             SPACE=(TRK,(1800,900),RLSE)
//SYSIN     DD *
  OPTION COPY
  OUTFIL FNAMES=(OUT1,OUT2,OUT3,OUT4,OUT5,
                 OUT6,OUT7,OUT8,OUT9,OUT0),SPLIT
//*********************************************************************
//* FILTRAR UN ARCHIVO
//*********************************************************************
//SORT001  EXEC PGM=SORT,PARM=('DYNALLOC=(SYSALLDA,32)')
//SORTIN   DD DSN=PRCQ.SCUENTAS.PART00.P22DS,DISP=SHR
//         DD DSN=PRCQ.SCUENTAS.PART02.P22DS,DISP=SHR
//SORTOUT  DD DSN=PRCQ.SCUENTAS.PART03.P22DS,
//            DISP=(,CATLG,DELETE),SPACE=(CYL,(500,100))
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
 SORT FIELDS=COPY
 INCLUDE COND=(I,L,T,C,V)
//*
//*I  Inicio. Posición donde empieza el campo por el que se quiere filtrar
//*L  Longitud máxima del campo por el que se quiere filtrar
//*T  Tipo de dato del campo que se quiere filtrar:
//*       CH - Carácter o numérico normal(sin COMP)
//*       BI - Hexadecimal (campos COMP)
//*C  Condición de la igualdad que se quiere realizar:
//*       EQ  Igual
//*       NE - Distinto
//*       GE  Mayor o igual
//*       GT - Mayor
//*       LE  Menor o igual
//*       LT - Menor
//*V  Valor del dato por el que se quiere filtrar
//*       X001A  Indica un valor hexadecimal o empaquetado
//*       CAL12  Indica un valor alfanumérico
//**********************************************
//*         FICHERO FB A VB
//* LA SALIDA MEDIRÁ 4 POSICIONES MÁS
//**********************************************
//SORT01   EXEC PGM=SORT
//SORTIN   DD DSN=fichero_de_entradaFB,DISP=SHR
//VBOUT    DD DSN=fichero_de_salidaVB,
//            DISP=(,CATLG,DELETE),
//            SPACE=(CYL,(200,50),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
    OPTION COPY
    OUTFIL FNAMES=VBOUT,FTOV
/*
//***********************************************
//*         FICHERO VB A FB
//* INDICAMOS LAS POSICIONES A COPIAR
//* PODEMOS COPIAR LAS POSICIONES QUE LLEVAN LA
//* INFORMACION DE LA LONGITUD DEL REGISTRO, O NO
//***********************************************
//SORT01   EXEC PGM=SORT
//SORTIN   DD DSN=fichero_de_entradaVB,DISP=SHR
//FBOUT    DD DSN=fichero_de_salidaFB,
//            DISP=(,CATLG,DELETE),
//            SPACE=(CYL,(200,50),RLSE)
//SYSOUT   DD SYSOUT=*
//SYSIN    DD *
    OPTION COPY
    OUTFIL FNAMES=FBOUT,VTOF,OUTREC(5,51)
/*
//*********************************************************************
//*  CRUZA 2 ARCHIVOS DE ENTRADA Y GENERA VARIOS FILTROS DE SALIDA
//*********************************************************************
//RCQ91601 EXEC PGM=ICETOOL
//TOOLMSG  DD SYSOUT=*
//DFSMSG   DD SYSOUT=*
//SHOWDEF  DD SYSOUT=*
//IN1      DD DSN=PRCQ.SCYB.D91600.BLANCKR.TELS.P22DS,DISP=SHR
//IN2      DD DSN=PRCQ.SCUENTAS.ALL.P22DS,DISP=SHR
//T1       DD DSN=&&T1,UNIT=SYSDA,SPACE=(TRK,(1500,500)),
//            DISP=(MOD,PASS)
//OUTVB    DD DSN=&&TVB,UNIT=SYSDA,SPACE=(TRK,(1500,500)),
//            DISP=(MOD,PASS)
//OUTB     DD DSN=&&TBB,UNIT=SYSDA,SPACE=(TRK,(1500,500)),
//            DISP=(MOD,PASS)
//OUTV     DD DSN=&&TVV,UNIT=SYSDA,SPACE=(TRK,(1500,500)),
//            DISP=(MOD,PASS)
//* COPIA TEMPORAL
//TOOLIN   DD *
  COPY FROM(IN1) TO(T1) USING(CTL1)
  COPY FROM(IN2) TO(T1) USING(CTL2)
  SPLICE FROM(T1) TO(OUTVB) ON(1,23,CH)  -
  WITH(1720,10) WITH(2501,1) WITHALL KEEPNODUPS USING(CTL3)
//* SPLICE. es la que antes de realizar el cruce ordene por la clave
//* especificada.
//* ON(posicion inicial, longitud, tipo). Especifica las claves de cruce.
//* WITH(posicion inicial, longitud). Especificaremos aquellos campos del
//* fichero IN2 que queramos que se vean en el fichero de salida.
//* El resto de posiciones del fichero de salida se rellenarán con los campos
//* del fichero IN1.
//* WITHALL. En el caso de que una clave esté más de una vez en el fichero IN2,
//* podemos especificar WITHALL para recoger todos los registros que la
//* contengan. Si no se especifica se quedaría con el primero.
//* KEEPNODUPS. Se utiliza para mostrar en el fichero de salida, los registro
//* del fichero IN1 cuya clave no existe en el fichero IN2.
//* USING. Podríamos decir que son unas "reglas de copiado" para definir el
//* fichero de salida. En este caso:
//* OUTFIL, para generar el fichero de salida OUT.
//* INCLUDE, para seleccionar aquellos registros que se cruzaron.
//* OUTREC, para eliminar las dos últimas posiciones de trabajo.
//CTL1CNTL DD *
  INREC BUILD=(1:1,2500,2501:C'BB')
//CTL2CNTL DD *
  INREC BUILD=(1:1,1729,2501:C'VV')
//* REGISTROS QUE ESTAN EN LOS DOS ARCHIVOS
//* REGISTROS QUE SOLO ESTAN EN IN1
//* REGISTROS QUE SOLO ESTAN EN IN2
//CTL3CNTL DD *
  OUTFIL FNAMES=OUTVB,INCLUDE=(2501,2,CH,EQ,C'VB'),OUTREC=(1,2500)
  OUTFIL FNAMES=OUTB,INCLUDE=(2501,2,CH,EQ,C'BB'),OUTREC=(1,2500)
  OUTFIL FNAMES=OUTV,INCLUDE=(2501,2,CH,EQ,C'VV'),OUTREC=(1,2500)
//*OUTFIL FNAMES=OUT2,INCLUDE=(12,2,CH,EQ,C'22'),BUILD=(1,10)
//*
//*************************************************************
//* OBJETIVO : COMPARA ARCHIVOS PARA OBTENER REG NO DUPLICADOS TEL
//*********************************************************************
//RCQ80105 EXEC PGM=ICETOOL,COND=(4,LT)
//TOOLMSG   DD SYSOUT=*
//DFSMSG    DD SYSOUT=*
//SYSPRINT  DD SYSOUT=*
//SYSOUT    DD SYSOUT=*
//*
//IN1       DD DSN=PRCQ.S001.RCQD8017.TELACT.SORT.P22DS,DISP=SHR
//          DD DSN=PRCQ.S001.RCQD8017.TELANT.SORT.P22DS,DISP=SHR
//OUT1      DD DSN=PRCQ.S001.RCQD8015.TELSORT.P22DS,
//             DCB=(RECFM=FB,LRECL=143,BLKSIZE=27885),
//             DISP=(NEW,CATLG,DELETE),
//             SPACE=(CYL,(500,200),RLSE)
//*
//TOOLIN   DD * DSN=&PARMS(RCQ8015A),DISP=SHR
   MODE STOP
   SELECT FROM(IN1) TO(OUT1) ON(2,142,CH)               -
                    NODUPS USING(CTL1)
//CTL1CNTL DD * DSN=&PARMS(RCQ8015B),DISP=SHR
   OUTFIL FNAMES=OUT1,
   INCLUDE=(1,1,CH,EQ,C'H')
//*
//********************************************************************* 02170000
//*  CONTADOR DE REGS DEL ARCHIVO RETIROS                             * 02180000
//********************************************************************* 02190000
//RCQ9880D EXEC PGM=ICEMAN                                              02200000
//SYSOUT   DD SYSOUT=*                                                  02201000
//SORTIN   DD DSN=QRCQ.SCYB.D98802.UNION.RETIROS.P22DS,DISP=SHR         02210000
//SORTOUT1 DD DSN=QRCQ.SCYB.D98802.UNION.RETIROS1.P22DS,                02220000
//             DISP=(NEW,CATLG,DELETE),                                 02230000
//             UNIT=SYSDA,                                              02240000
//             SPACE=(CYL,(1,1),RLSE),                                  02250000
//             DCB=(BLKSIZE=27979,LRECL=49,RECFM=FBA,BUFNO=30)          02260000
//SYSIN    DD *                                                         02270000
  SORT FIELDS=COPY                                                      02280000
* CREATE REPORT                                                         02290000
  OUTFIL FNAMES=SORTOUT1,NODETAIL,                                      02300000
  TRAILER1=('TRAILER:',COUNT=(M11,LENGTH=10))
//*************************************************************
//*             DESCARGA DATOS DE TABLA
//***********************************************************
//RESPAL40 EXEC PGM=IKJEFT01,DYNAMNBR=20,TIME=1440,COND=(4,LT)
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
  DSN SYSTEM(DSN)
  RUN PROGRAM(DSNTIAUL) PLAN(DSNTIAUL) PARMS('SQL')
  END
//*PARA OAT
//*  DSN SYSTEM(DB53)
//*  RUN PROGRAM(DSNTIAUL) PLAN(DSNTIAUL) PARMS('SQL')
//* PARA PROD
//*     SYSTSIN  DD DSN=&INLIB(DSNTIAUL),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//* DONDE SE DEPOSITA LA DESCARGA
//SYSREC00 DD DSN=DCP4.SNV.TELEFONO.SYSREC00,
//       SPACE=(CYL,(1,1),RLSE),DISP=(SHR,CATLG,DELETE)
//* LAYOUT DE LA DESCARGA
//SYSPUNCH DD DSN=DCP4.SNV.TELEFONO.SYSPUNCH,
//      SPACE=(CYL,(1,1),RLSE),DISP=(SHR,CATLG,DELETE)
//SYSIN DD *
   SELECT * FROM RCRD101.TE_DIRE;
//***********************************************************
//*             CARGA DATOS EN TABLA
//***********************************************************
//STEP0001  EXEC DSNPROC,SYSTEM=DSN,UID='LOADTB',
//          UTPROC='',COND=(0,LT)
//SORTWK01 DD DSN=&&SORTWK01,
//     SPACE=(TRK,(12500,12500),RLSE),DISP=(MOD,DELETE,CATLG)
//SORTWK02 DD DSN=&&SORTWK02,
//     SPACE=(TRK,(12500,12500),RLSE),DISP=(MOD,DELETE,CATLG)
//SORTWK03 DD DSN=&&SORTWK03,
//     SPACE=(TRK,(12500,12500),RLSE),DISP=(MOD,DELETE,CATLG)
//SORTWK04 DD DSN=&&SORTWK04,
//     SPACE=(TRK,(12500,12500),RLSE),DISP=(MOD,DELETE,CATLG)
//SORTWK05 DD DSN=&&SORTWK05,
//     SPACE=(TRK,(12500,12500),RLSE),DISP=(MOD,DELETE,CATLG)
//SORTWK06 DD DSN=&&SORTWK06,
//     SPACE=(TRK,(12500,12500),RLSE),DISP=(MOD,DELETE,CATLG)
//SORTWK07 DD DSN=&&SORTWK07,
//     SPACE=(TRK,(12500,12500),RLSE),DISP=(MOD,DELETE,CATLG)
//*
//* LOS REGISTROS A CARGAR
//SYSREC DD DISP=SHR,DSN=DCP4.SNV.TELEFONO.SYSREC00
//*
//SYSUT1 DD DSN=&&SYSUT1,SPACE=(CYL,(2500,2500),RLSE),
//     DISP=(MOD,DELETE,CATLG)
//SORTOUT DD DSN=&&SORTOUT,
//     DISP=(MOD,DELETE,CATLG),SPACE=(CYL,(2500,2500),RLSE)
//SYSERR DD DSN=&&SYSERR,SPACE=(TRK,(150,15),RLSE),
//     DISP=(MOD,DELETE,CATLG)
//SYSDISC DD DSN=&&SYSDISC,SPACE=(TRK,(150,15),RLSE),
//     DISP=(MOD,DELETE,CATLG)
//SYSMAP DD DSN=&&SYSMAP,SPACE=(TRK,(12500,12500),RLSE),
//     DISP=(MOD,DELETE,CATLG)
//*   LAYOUT DE LA TABLA DONDE SE CARGARAN LOS DATOS
//SYSIN   DD DISP=SHR,DSN=DCP4.SNV.TELEFONO.SYSPUNCH
//*************************************************************
//*      CORRER UPDATE DE DB2
//*************************************************************
//RCQ01001 EXEC PGM=IKJEFT01,REGION=8M,TIME=1440,DYNAMNBR=20
//SNAPER   DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*
//SYSPRINT DD  SYSOUT=*
//SYSTSPRT DD  SYSOUT=*
//SYSPUNCH DD  DUMMY
//SYSUDUMP DD  SYSOUT=*
//SYSTSIN  DD *
    DSN SYSTEM (DB50)
    RUN PROGRAM(DSNTEP2) PLAN(DSNTEP2)
    END
//SYSIN    DD *
    UPDATE RCRP101.TE_TELE
        SET TELE_TIPO = 04
    WHERE TELE_PREFIJO = '01'
    AND TELE_TIPO = 03;
    COMMIT;
//*
//***********************************************************
//*      EJECUCION DE COBOL DB2
//**********************************************************************
//PASO1 EXEC PGM=IKJEFT01,DYNAMNBR=20
//PARAMETR DD DSN=DKRR.SSPARMRX.P22DS,DISP=SHR
//PASO     DD DSN=DKRN.SSPASO01.SIP2CIF.P22DS,DISP=(NEW,DELETE,DELETE),
//            DCB=(RECFM=FB,LRECL=22,BLKSIZE=27984),
//            SPACE=(CYL,(1,1))
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSABOUT DD SYSOUT=*
//SYSDBOUT DD SYSOUT=*
//ABENDAID DD SYSOUT=*
//STDIN    DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//STDERR   DD SYSOUT=*
//SYSDUMP  DD SYSOUT=*
//SYSABEND DD SYSOUT=*
//CEEDUMP  DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//* DESARROLLO
//SYSTSIN  DD *
 DSN SYSTEM(DSN)
 RUN PROGRAM(RCQP043) PLAN(DRCQBMP1) PAR('000101000')
//* PROD
//*DSN SYSTEM(DB50) RUN PROGRAM(RCQP043) PLAN(PRCQBMP1) PAR('0001')
//* OAT
//*DSN SYSTEM(DB53) RUN PROGRAM(RCQP043) PLAN(QRCQBMP1) PAR('0001')
//*************************************************************
//*         EJECUCION DE COBOL SIN DB2
//*************************************************************
//RCRE0303 EXEC PGM=RCRE031,COND=(4,LT)
//INFILE01 DD DSN=PRCR.SNV.TEDIRE.P22DS,
//           DISP=SHR
//OUTFILE1 DD DSN=PRCR.SNV.REFDIRE.P22DS,
//           DISP=(NEW,CATLG,DELETE),
//           DCB=(RECFM=FB,LRECL=787,BLKSIZE=0),
//           SPACE=(CYL,(600,400),RLSE)
//SYSPRINT DD SYSOUT=*
//SYSDBOUT DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//*
//*********************************************************************
//*    FUNCION: ENVIO DE ARCHIVOS DE MEMOS A CTA                      *
//*********************************************************************
//PMBATCH1 EXEC PGM=DMBATCH,REGION=1024K,PARM=(YYSLYNN),COND=(4,LT)
//STEPLIB  DD DISP=SHR,DSN=SYS2.CD.LINKLIB
//DMNETMAP DD DISP=SHR,DSN=SYS7.CD.PLB2.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=SYS7.CD.PLB2.PROCESS
//DMMSGFIL DD DISP=SHR,DSN=SYS7.CD.PLB2.MSG
//DMPRINT  DD SYSOUT=*
//SYSIN    DD *
  SIGNON
  SUBMIT PROC = ENLAPAMX                                       -
   &SDSN=PRCQ.SNV.MEMOS.SCARD.P22DS                            -
   &RDSN=POCL3.CD091.MEX.HCMMI                                 -
    CASE=YES
    SIGNOFF
//*  EOF   FIN DE PROCEDIMIENTO
//**********************************************************
//*   VALIDAR TRANSFERENCIA REALIZADA
//**********************************************************
//DMBATCH EXEC PGM=DMBATCH,REGION=1024K,PARM=(YYSLYNN)
//STEPLIB DD DISP=SHR,DSN=SYS2.CD.LINKLIB
//DMNETMAP DD DISP=SHR,DSN=SYS7.CD.DL12.NETMAP
//DMPUBLIB DD DISP=SHR,DSN=SYS7.CD.DL12.PROCESS
//DMMSGFIL DD DISP=SHR,DSN=SYS7.CD.DL12.MSG
//DMPRINT  DD SYSOUT=*
//*
//SYSIN    DD *
  SIGNON
      SEL STAT WHERE ( -
                   STARTT=(17/04/2017)    -
                   STOPT=(17/04/2017)     -
                   EXCLUDE=(WTO,MEMB)     -
                   PNUM=(93702)       -
                   SNODE=(CD.GRD1HK)  -
                   )
  SIGNOFF
//*********************************************************************
//*  OBTENER DIFERENCIAS ENTRE ARCHIVOS
//*---------------------------
//* MARCA ARCHIVOS PARA SUMAR DUPLICADOS (DE PREFERENCIA NO DEBE HABER
//* DUPLICADOS EN LOS ARCHIVOS ORIGENES
//*---------------------------
//RCQ1604 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=QRCQ.S001.OAYER.P22DS,DISP=SHR
//         DD DSN=QRCQ.S001.OHOY.P22DS,DISP=SHR
//SORTOUT  DD DSN=QRCQ.S001.ODUPLI.P22DS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(35000,5000),RLSE),
//            DCB=(RECFM=FB,LRECL=25,BLKSIZE=27975)
//SYSIN    DD *
  SORT FIELDS=(1,19,CH,A)
  INREC OVERLAY=(25:C'1')
//*---------------------------
//*  SUMA CUENTAS QUE COINCIDAN PARA IDENTIFICAR DUPLICADOS
//*---------------------------
//RCQ1603 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=QRCQ.S001.ODUPLI.P22DS,DISP=SHR
//SORTOUT  DD DSN=QRCQ.S001.OFINAL.P22DS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(3500,5000),RLSE),
//            DCB=(RECFM=FB,LRECL=25,BLKSIZE=27975)
//SYSIN    DD *
  SORT FIELDS=(1,19,ZD,A)
  SUM FIELDS=(25,1,ZD)
//*---------------------------
//* SELECCIONA LAS CUENTAS QUE SOLO ESTEN EN 'AYER',
//* DONDE LA SUMA DEL PASO ANTERIOR SIGA SIENDO 1
//*---------------------------
//RCQ1602 EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=QRCQ.S001.OFINAL.P22DS,DISP=SHR
//SORTOUT  DD DSN=QRCQ.S001.OFILTRO.P22DS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1500,500),RLSE),
//            DCB=(RECFM=FB,LRECL=25,BLKSIZE=27975)
//SYSIN    DD *
  SORT FIELDS=COPY
  INCLUDE COND=(21,2,CH,EQ,C'11',AND,25,1,CH,EQ,C'1')
//*********************************************************************
//**************************************************************
//*  CONTADOR DE REGS DEL ARCHIVO RETIROS
//**************************************************************
//RCQ9880D EXEC PGM=ICEMAN
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=QRCQ.SCYB.D98802.UNION.RETIROS.P22DS,DISP=SHR
//SORTOUT1 DD DSN=QRCQ.SCYB.D98802.UNION.RETIROS1.P22DS,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(CYL,(1,1),RLSE),
//             DCB=(BLKSIZE=27979,LRECL=49,RECFM=FBA,BUFNO=30)
//SYSIN    DD *
  SORT FIELDS=COPY
* CREATE REPORT
  OUTFIL FNAMES=SORTOUT1,NODETAIL,
  TRAILER1=('TRAILER:',COUNT=(M11,LENGTH=10))
