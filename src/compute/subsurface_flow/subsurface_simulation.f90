MODULE subsurface_simulation
!----------------------------------------------------------------------*
! Main simulation driver and time-stepping logic
! Contains VSSIM - the main VSS timestep controller
!----------------------------------------------------------------------*
! Extracted from VSmod.f90 as part of refactoring
! Date: 2025-09-04
! Source: VSmod.f90.sav (lines 3520-4004)
!----------------------------------------------------------------------*
   USE SGLOBAL
   USE subsurface_variables
   USE subsurface_column_solver, ONLY : VSCOLM
   USE subsurface_boundary_conditions, ONLY : VSPREP
   USE subsurface_utilities, ONLY : VSMB
   USE AL_G, ONLY : ICMREF, NX, NY, NGDBGN
   USE AL_C, ONLY : BHB, BFB, bexbk, DTUZ, deltaz, dummy, DHF, ESOILA, ERUZ, EEVAP, &
      FHBED, ISORT, jvsacn, JVSDEL, idum, icmbk, LFB, LHB, LINKNS, lgb, &
      NWELBT, NWELTP, NVSSPC, NVSWLI, NTSOIL, nhbed, NVC, NRD, nlyrbt, NVSWLT, NVSSPT, NBFACE, NS, nlyr, &
      PNETTO, QVSSPR, QVSBF, QH, QVSWEL, QBKF, QBKB, QVSV, QVSWLI, QVSH, QBKI, &
      tih, UZNEXT, &
      vsd, VSI, VSPSI, VSTHE, VSPOR, WLD, ZVSPSL, zlyrbt, zvsnod, zbeff, INITIALISE_AL_C, TIH
   USE AL_D, ONLY : TTH
   USE UTILSMOD, ONLY : DCOPY
   USE OCmod2, ONLY : GETHRF
   USE mod_load_filedata, ONLY : ALINIT
   IMPLICIT NONE

   PRIVATE
   ! Public simulation routine
   PUBLIC :: VSSIM

CONTAINS

!SSSSSS SUBROUTINE VSSIM ()
   SUBROUTINE VSSIM ()
!----------------------------------------------------------------------*
! VSS Controlling routine for a single timestep
!----------------------------------------------------------------------*
! Version:  SHETRAN/VSS/VSSIM/4.2
! Modifications:
!  GP  29.07.94  written (v4.0 finished 17.07.96)
! RAH  961228  4.1  Remove temporary debug code.  DPSIEL,DPSIMX >=0.
!                   Bring CWV,CWL from VSCOLM.INC, and pass to VSCOLM.
!      970207       Dispense with CNOW,CTHEN,CV,CWV,CWL,VSPOR1,VSSTMP.
!                   Replace CQINF with CQV(ICTOP).  Use DO 660 not GOTO.
!                   CQWI is redefined - see VSWELL.  asum QVSWEL locally.
!                   Use OK to simplify convergence test.
!      970210       Remove CETAN,CKRN.CPSIM,NVSCIT. Make PSIM 1D not 2D.
!                   Dispense with BCHELE,CA0,CPSIN,CPSL,CQSP,CZG,DT.
!                   Use ALINIT and DCOPY.  Bring VSPSIN,VSTHEN from
!                   VSCOM1.INC and reverse indices.  If JEL.le.0 set
!                   JCACN=0, and don't set JCDEL* or C*IJ1,CZ1,CPSI*1.
!                   Move CQH initialization to VSCOLM.  Move SIGMA from
!                   VSCOLM.INC to VSINTC.  Set ICTOP,QH,QVSBF,QBK* once.
!      970211       Replace CES,CDW,CEW,CQP with CDNET, bring CQ from
!                   VSCOLM.INC (with ICBED,ICLYRB,ICSOIL), add dim, set
!                   once. Scrap ICWL*,ICSP*,CZSP,CCS.Initialize QH,QVSH.
!      970213       VSCOLM.INC: bring JCBC,ICWCAT,ICLBCT,ICBBCT,CZS, and
!                   scrap CQWIN,CLF,ICLFL,ICLFN,CLH,ICLHL,ICLHN,CLG,
!                   ICLGL,ICLGN,CBF,CBH. Include VSSOIL.INC. rm NVSSPT,
!                   NVSWLT (AL.C).  JCBC: add dimension; define once.
!                   Swap subscripts on NVSL*L,RL*NOW (in VSCOM1.INC).
!      970214       Bring from VSCOLM.INC: CDELL,CDELL1,CAIJ,CAIJ1.
!                   Replace CAIJ with VSAIJ: set once; use for CAIJ1.
!                   Reverse DELTAZ,QVSH subscripts (in AL.C); pass to
!                   VSCOLM; scrap CDELZ,CQH.
!      970217       Swap subscripts: JVSACN,JVSDEL,ZVSNOD,QVSV,QVSWLI,
!                   VSPSI,VSTHE (see AL.C), & IVSSTO,VSKR (VSCOM1.INC)
!                   (also fixes error whereby ICSTOR not initialized).
!                   VSCOLM.INC: scrap JCACN,JCDEL,CZ,CQV,CQWI,CPSI,
!                   ICSTOR,CTHETA,CKR; bring remainder (CPSI1,CPSIN1,
!                   CZ1,CKIJ1,JCDEL1).  Add dimension to ICSOIL: set
!                   once; scrap CKZS,CKIJS (use VSK3D); use for CKIJ1.
!                   Redefine CQ: multiply by AREA*DELTAZ (see VSINTC).
!                   Move QVSWEL outside loop.  VSMB straight after loop.
!      970515       Re-order VSCOLM arguments.
!      970522       Don't need MAX for ICWLBT,etc.
!      970618       Don't call VSCOLP.  DO 285 if JEL.GE.ISTART (was 1).
! RAH  980402  4.2  Pass new local ELEVEL to VSCOLM.
!JE   JAN 2009      Loop restructure for AD
!----------------------------------------------------------------------*
! Entry conditions:
!   1 <= LLEE,  NEL,   NLFEE
!  LL <= LLEE;  NEL <= NELEE;  0 <= NLF <= NLFEE
! for each e in 1:NEL:    LLEE >= LL =NLYRBT(e,NLYR(e)+1)
!     for each layer in 1:NLYR(e): 1<=NLYRBT(e,layer)<=NLYRBT(e,layer+1)
! for each link in 1:NLF: for each face in 1:4:
!    BEXBK  ==>  1 <= ICMREF(jel,1) <= 3,  where jel=ICMREF(link,face+4)
! for each e in istart:NEL: 0 <= NLBTYP(e), NBBTYP(e)
!                           1 <= NLBCAT(e), NBBCAT(e), NVSWLC(e)
!                           1 <= NWELBT(e) <= NWELTP(e) <= LLEE
!                           4 >= NBFACE(e)
!                       NVSEE >= NVSWLI(e)
!         NLBTYP(e)>0  ==>  0 <  NBFACE(e)
!                           0  = NVSWLI(e)*NVSSPC(e)
!     for each face in 1:4:
!         ICMREF(e,4+face)<istart ==> JVSACN(face,NLYRBT(e,1):LL,e) = 0,
!  where istart=1 if BEXBK, NLF+1 otherwise
! for each e in NLF+1:NEL:  1 <= NVC(e) <= NV (size of NRD array)
!            LL-NLYRBT(e,1)+1 >= NRD(veg),
!                              where veg=NVC(e)
! ...
!----------------------------------------------------------------------*
! Limited ranges:
!                    range (e,1:LLEE):  (e,NLYRBT(e,1):LL) only
!                    range    1:NLFEE:               1:NLF only
! JVSACN(face,cell,e), NLYR(e), NTSOIL(e,layer), NVSWLI(e):
!              for e in istart:NEL only, where istart is defined above
! VSKR(e,cell): input for any e having a neighbour earlier in ISORT list
!              output for e in istart:NEL
! ...
!----------------------------------------------------------------------*
! Commons and constants
! Imported constants
!     AL.P.VSS:        LLEE,NELEE,NLFEE,NLYREE,NSEE,NVSEE
!     VSSOIL.INC:      NSOLEE
! Input common
!     AL.C:            BFB,BHB,LFB,LGB,LHB,LL,NEL,NLF,NS,PRI,WLD,ERR
!                      JVSACN(4,LLEE,NEL),JVSDEL(4,LLEE,NEL)
!                      NHBED(NLFEE,2),NLYR(NEL),NLYRBT(NELEE,NLYREE)
!                      NRD(*),NTSOIL(NELEE,NLYREE),NVC(NLF+1:NEL)
!                      NVSSPC(NEL),NVSWLI(NEL),NWELBT(NEL),NWELTP(NEL)
!                      TIH,AREA(NEL),DELTAZ(LLEE,NEL),DHF(NELEE,4)
!                      VSPOR(NS),ZGRUND(NEL),ZVSNOD(LLEE,NEL)
!                      BEXBK,LINKNS(NLFEE)
!                      ISORT(NEL)
!                      DTUZ,UZNEXT,UZNOW,EEVAP(NEL)
!                      ERUZ(NELEE,LL),ESOILA(NEL),HRF(NEL),PNETTO(NEL)
!     AL.G:            ICMREF(NELEE,12)
!     VSCOM1.INC:      NVSBF,NVSBH, NLBCAT(NEL),  NLBTYP(NEL)
!                      NVSLF,NVSLFT,NVSLFN(NVSEE),NVSLFL(NLYREE,NVSEE)
!                      NVSLG,NVSLGT,NVSLGN(NVSEE),NVSLGL(NLYREE,NVSEE)
!                      NVSLH,NVSLHT,NVSLHN(NVSEE),NVSLHL(NLYREE,NVSEE)
!                      NVSWL,NVSWLC(NEL),         NBBTYP(NEL)
!                      VSWL,VSWV,VSK3D(NSEE,3),VSSPZ(NEL),VSSPCO(NEL)
!                      BHELEV
!     VSSOIL.INC:      NVSSOL
!                      VSPDET(NSOLEE,NS),VSPDKR(NSOLEE,NS)
!                      VSPETA(NSOLEE,NS), VSPKR(NSOLEE,NS)
!                      VSPPSI(NVSSOL),   VSPTHE(NSOLEE,NS)
! In+out common
!     AL.C:             VSPSI(LLEE,NEL),VSTHE(LLEE,NEL)
!     VSCOM1.INC:      IVSSTO(LLEE,NEL), VSKR(LLEE,NEL)
! Output common
!     AL.C:            QBKB(NLFEE,2),    QH(NEL),ZVSPSL(NEL)
!                      QBKF(NLFEE,2), QVSBF(NEL),QVSWLI(LLEE,NVSEE)
!                      QBKI(NLFEE,2),QVSSPR(NEL),  QVSV(LLEE,NEL)
!                                    QVSWEL(NEL),  QVSH(4,LLEE,NELEE)
! Workspace common
!     VSCOM1.INC:      RBFNOW(NVSEE),RLFNOW(NLYREE,NVSEE)
!                      RBHNOW(NVSEE),RLGNOW(NLYREE,NVSEE)
!                       WLNOW(NVSEE),RLHNOW(NLYREE,NVSEE)
      INTEGER :: NITMAX, NITMIN
      DOUBLEPRECISION GEPSMX, DRYH
      PARAMETER (NITMAX = 10, NITMIN = 2, GEPSMX = 1D-4, DRYH = 1D-8)
      INTEGER :: N, IFDUM1, IFDUM2, NIT, NCELL, WET, ICDUM, K, ELEVEL
      INTEGER :: I, II, IEL, IFA, ICL, ILYR, IW, ITYPE, IBK, ISTART, &
         IBANK
      INTEGER :: JEL, JFA, JCL, JCBED, JELDUM (4), JCDEL1 (LLEE, 4)
      INTEGER :: ICBOT, ICTOP, ICWCAT, ICLBCT, ICBBCT, ICBED, ICWLBT
      INTEGER :: ICLYRB (NLYREE)
      DOUBLEPRECISION DPSIEL, DPSIMX, DELTAP (0:NELEE)
      DOUBLEPRECISION CDW, CES, CQW, CDNET (NELEE), CQ (LLEE, NELEE), &
         QBK, QI
      DOUBLEPRECISION CA0, CDELL (4), CDELL1 (4), CAIJ1 (LLEE, 4), &
         CZ1 (LLEE, 4)
      DOUBLEPRECISION DXYDUM
      DOUBLEPRECISION PSIM (LLEE), VSPSIN (LLEE, NELEE), VSTHEN (LLEE, &
         NELEE)
      DOUBLEPRECISION CPSI1 (LLEE, 4), CPSIN1 (LLEE, 4), CKIJ1 (LLEE, 4) &
         , CZS (4)
      integer,save :: errorcount2=0
!!!!!! Extra array: depadj - depth of surface water for adjacent
! elements - added for channel aquifer flows fix, SPA, 03/11/98
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      DOUBLEPRECISION depadj (4)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      LOGICAL :: TEST, OK (NELEE), g670
!----------------------------------------------------------------------*
! Initialization
!________________*
      IF (BEXBK) THEN
         IBANK = 1
         ISTART = 1
      ELSE
         IBANK = 0
         ISTART = total_no_links + 1
      ENDIF

      ICTOP = top_cell_no
      IF (FIRSTvssim) THEN

         FIRSTvssim = .FALSE.
!         * set outputs & locals for non-column elements
         IF (ISTART.GT.1) CALL ALINIT (ZERO, ISTART - 1, QH)
         DO 4 IEL = 1, ISTART - 1
            ICBOT = NLYRBT (IEL, 1)
            N = 4 * (ICTOP - ICBOT + 1)
            CALL ALINIT (ZERO, N, QVSH (1, ICBOT, IEL) )
            CALL ALINIT (ZERO, N, VSAIJsv (1, ICBOT, IEL) )
            DO ICL = ICBOT, ICTOP
               ICSOILsv (ICL, IEL) = 1
            END DO

4        END DO
!        * set static locals for column elements
         DO 95 IEL = ISTART, total_no_elements
! JCBC contains boundary condition types:
! 0 - bottom boundary; 1-4 - faces; 5 - well/spring
! boundary condition types are:
! 0     internal face or no-flow boundary condition
! 1     wells
! 2     springs
! 3     lateral flow
! 4     lateral head
! 5     lateral head gradient
! 6     column base flow
! 7     column base head
! 8     column base free drainage
! 9     stream-aquifer interaction (without banks)
! 10    stream-aquifer interaction (with banks)
            DO II = 1, 5
               JCBCsv (II, IEL) = 0
            END DO
            JCBCsv (0, IEL) = NBBTYP (IEL)
            IFA = MAX (1, NBFACE (IEL) )
            JCBCsv (IFA, IEL) = NLBTYP (IEL)
            IF (NVSWLI (IEL) .GT.0) JCBCsv (5, IEL) = 1
            IF (NVSSPC (IEL) .GT.0) JCBCsv (5, IEL) = 2
            DO 90 IFA = 1, 4
               JEL = ICMREF (IEL, IFA + 4)
               TEST = IEL.GT.total_no_links.AND.JEL.GE.1.AND.JEL.LE.total_no_links
               IF (TEST) JCBCsv (IFA, IEL) = 9 + IBANK
! VSAIJ contains cell-face areas for lateral flow (note face 1=3, 2=4)
               IFDUM1 = MOD (IFA, 4) + 1
               IFDUM2 = MOD (IFA + 2, 4) + 1
               DXYDUM = DHF (IEL, IFDUM1) + DHF (IEL, IFDUM2)
               DO ICL = NLYRBT (IEL, 1), ICTOP
                  VSAIJsv (IFA, ICL, IEL) = DELTAZ (ICL, IEL) * DXYDUM
               END DO
90          END DO
! ICSOIL contains soil types for each cell
            DO 93 ILYR = 1, NLYR (IEL)
               N = NTSOIL (IEL, ILYR)
               DO ICL = NLYRBT (IEL, ILYR), NLYRBT (IEL, ILYR + 1) &
                  - 1
                  ICSOILsv (ICL, IEL) = N
               END DO
93          END DO

95       END DO



      ENDIF
! prepare catchment boundary condition data

      CALL VSPREP
!!!!!! Calc. depth of water for channel links, even if no banks
! n.b. rainfall and evap terms neglected, as these are calculated for
! channels after VSS is called.
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (.not.bexbk) then
         do 107 iel = 1, total_no_links
            cdnet (iel) = GEThrf (iel) - zgrund (iel)
107      end do

      endif
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      DO 108 IEL = ISTART, total_no_elements

         CES = ESOILA (IEL)
         CDW = GETHRF (IEL) - ZGRUND (IEL)

         CDNET (IEL) = (PNETTO (IEL) - (EEVAP (IEL) - CES) ) * DTUZ + &
            CDW
         CA0 = cellarea (IEL)
         ICBOT = NLYRBT (IEL, 1)
         ICDUM = ICTOP + 1
         IF (IEL.GT.total_no_links) ICDUM = ICDUM - NRD (NVC (IEL) )
         IF (ICDUM.GT.ICBOT) CALL ALINIT (ZERO, ICDUM - ICBOT, CQ ( &
            ICBOT, IEL) )

! stop crash if rooting zone is below base of aquifer sb 020211
         icdum=max(1,icdum)

         DO 106 ICL = ICDUM, ICTOP
            CQ (ICL, IEL) = - ERUZ (IEL, ICL) * CA0
106      END DO

         CQ (ICTOP, IEL) = CQ (ICTOP, IEL) - CES * CA0



108   END DO
! save psi values at time level N
      DO 212 IEL = 1, total_no_elements
         ICBOT = NLYRBT (IEL, 1)
         NCELL = ICTOP - ICBOT + 1
         CALL DCOPY (NCELL, VSPSI (ICBOT, IEL), 1, VSPSIN (ICBOT, IEL), &
            1)
         CALL DCOPY (NCELL, VSTHE (ICBOT, IEL), 1, VSTHEN (ICBOT, IEL), &
            1)



212   END DO
! initialize convergence indicators
      CALL ALINIT (ZERO, ISTART, DELTAP)
      DO IEL = 1, ISTART - 1
         OK (IEL) = .TRUE.
      END DO
      DO IEL = ISTART, total_no_elements
         OK (IEL) = .FALSE.
      END DO
! start of main iteration loop
!______________________________*
      ELEVEL = 0

      g670=.FALSE.
      out660 : DO NIT = 1, NITMAX
         IF(g670) CYCLE
         IF (NIT.EQ.NITMAX) ELEVEL = EEERR
         DPSIMX = ZERO

         DO 500 I = 1, total_no_elements
            IEL = ISORT (I)
            IF (.NOT. OK (IEL) ) THEN
               ICBOT = NLYRBT (IEL, 1)
               ITYPE = ICMREF (IEL, 1)


               NCELL = ICTOP - ICBOT + 1
! save psi at iteration level m


               CALL DCOPY (NCELL, VSPSI (ICBOT, IEL), 1, PSIM (ICBOT), &
                  1)
! set up column arrays using global arrays
               DO ILYR = 1, NLYR (IEL) + 1
                  ICLYRB (ILYR) = NLYRBT (IEL, ILYR)
               END DO

               IF (ITYPE.EQ.1.OR.ITYPE.EQ.2) ICBED = NHBED (ICMREF (IEL, 4) &
                  , ITYPE)

               DO 300 IFA = 1, 4
                  CDELL (IFA) = DHF (IEL, IFA)
                  JEL = ICMREF (IEL, IFA + 4)

                  JELDUM (IFA) = JEL
                  IF (JEL.LT.1) THEN
                     DXYDUM = ZERO
                  ELSE

                     CZS (IFA) = GETHRF (JEL)
! !!!!! fix for channel aquifer flows, SPA, 03/11/98
! Pass depth of water in adjacent elements to vscolm
! as well as elevation of water surface
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                     depadj (ifa) = cdnet (jel)
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                     JFA = ICMREF (IEL, IFA + 8)
                     DXYDUM = DHF (JEL, JFA)
                  ENDIF
                  CDELL1 (IFA) = DXYDUM

                  IF (JEL.GE.ISTART) THEN
!              NB: VSPSI, VSKR may hold values from previous iteration
                     K = MOD (JFA - 1, 2) + 1
                     DO 285 JCL = NLYRBT (JEL, 1), top_cell_no
                        JCDEL1 (JCL, IFA) = JVSDEL (JFA, JCL, JEL)
                        CAIJ1 (JCL, IFA) = VSAIJsv (JFA, JCL, JEL)
                        CZ1 (JCL, IFA) = ZVSNOD (JCL, JEL)
                        CPSI1 (JCL, IFA) = VSPSI (JCL, JEL)
                        CPSIN1 (JCL, IFA) = VSPSIN (JCL, JEL)
                        N = ICSOILsv (JCL, JEL)
                        CKIJ1 (JCL, IFA) = VSKR (JCL, JEL) * VSK3D (N, K)

285                  END DO
                  END IF


300            END DO
! boundary condition indices
               IW = MAX (1, NVSWLI (IEL) )
               ICWLBT = NWELBT (IEL)
               ICWCAT = NVSWLC (IEL)
               ICLBCT = NLBCAT (IEL)


               ICBBCT = NBBCAT (IEL)
! calculate new potentials and flow rates



               CALL VSCOLM (NSEE, VSWV, VSWL, VSK3D, BHELEV, ELEVEL, &
                  IEL, ICBOT, ICTOP, ICBED, ICLYRB, ICSOILsv (ICBOT, IEL), &
                  JCBCsv (0, IEL), JCDEL1, JELDUM, JVSACN (1, ICBOT, IEL), &
                  JVSDEL (1, ICBOT, IEL), NVSSPC (IEL), NVSLFN (ICLBCT), &
                  NVSLFL (1, ICLBCT), NWELBT (IEL), NVSLHN (ICLBCT), NVSLHL ( &
                  1, ICLBCT), NWELTP (IEL), NVSLGN (ICLBCT), NVSLGL (1, &
                  ICLBCT), cellarea (IEL), ZGRUND (IEL), VSSPZ (IEL), VSSPCO (IEL) &
                  , DELTAZ (ICBOT, IEL), ZVSNOD (ICBOT, IEL), CDELL, VSAIJsv (1, &
                  ICBOT, IEL), CAIJ1, CDELL1, CZ1, DTUZ, CDNET (IEL), VSPSIN ( &
                  ICBOT, IEL), CQ (ICBOT, IEL), CZS, CPSI1, CPSIN1, CKIJ1, &
                  WLNOW (ICWCAT), RLFNOW (1, ICLBCT), RLHNOW (1, ICLBCT), &
                  RLGNOW (1, ICLBCT), RBFNOW (ICBBCT), RBHNOW (ICBBCT), &
                  IVSSTO (ICBOT, IEL), VSPSI (ICBOT, IEL), VSKR (ICBOT, IEL), &
                  VSTHE (ICBOT, IEL), QVSH (1, ICBOT, IEL), QVSV (ICBOT - 1, &
                  IEL), QVSWLI (ICWLBT, IW), QVSSPR (IEL), ZVSPSL (IEL), &
                  depadj)
!!!!!! extra argument depadj added for channel-aquifer flows fix
! SPA, 03/11/98
! record largest change for this iteration
               DPSIEL = ZERO
               DO ICL = ICBOT, ICTOP
                  DPSIEL = MAX (DPSIEL, ABS (VSPSI (ICL, IEL) - PSIM (ICL) ) )
               END DO
               DELTAP (IEL) = DPSIEL



               DPSIMX = MAX (DPSIMX, DPSIEL)
! end of element loop: check for convergence or maximum iterations
            END IF
500      END DO
!970214  At present the criterion on DPSIMX overrides that on NIT
         IF (DPSIMX.LE.GEPSMX) THEN
            g670 =.TRUE.
            CYCLE out660
         ENDIF
         IF (NIT.GE.NITMIN) THEN
            DO 650 IEL = ISTART, total_no_elements
               DPSIEL = DELTAP (IEL)
               DO 640 IFA = 1, 4
                  JEL = MAX (0, ICMREF (IEL, IFA + 4) )
                  DPSIEL = MAX (DPSIEL, DELTAP (JEL) )
640            END DO
               OK (IEL) = DPSIEL.LT.GEPSMX
650         END DO



         ENDIF
! end of iteration loop
      ENDDO out660
      IF(.NOT.g670) then
         errorcount2=errorcount2+1
         if (errorcount2.lt.errcntallowed) then
            CALL ERROR(EEERR, 1039, PPPRI, 0, 0, 'Maximum iterations in VSS global solver')
         elseif (errorcount2.eq.errcntallowed) then
            CALL ERROR (EEERR, 1039, PPPRI, 0, 0, '**** Last printout of the error message - maximum iterations in VSS global solver *****')
         endif
      endif




670   CONTINUE
! main solution is complete: tidy up
!____________________________________*
! update flows to ensure mass conservation


      CALL VSMB (VSTHEN)
! set auxiliary output arrays
      DO 700 IEL = ISTART, total_no_elements
         ICBOT = NLYRBT (IEL, 1)
         QVSBF (IEL) = QVSV (ICBOT - 1, IEL)
         QH (IEL) = QVSV (ICTOP, IEL)
         IW = NVSWLI (IEL)
         IF (IW.GE.1) THEN
            CQW = ZERO
            DO ICL = NWELBT (IEL), NWELTP (IEL)
               CQW = QVSWLI (ICL, IW) + CQW
            END DO
            QVSWEL (IEL) = CQW
         END IF


700   END DO
! calculate QBKB, QBKF, QBKI for all cases:
!     bank elements or not, including dry channels
      DO 780 IBK = 1, 2

         DO 760 IEL = 1, total_no_links
            QI = - HALF * cellarea (IEL) * QH (IEL)

            WET = NINT (HALF + SIGN (HALF, GETHRF (IEL) - ZGRUND (IEL) &
               - DRYH) )
            IFA = 2 * IBK

            IF (LINKNS (IEL) ) IFA = IFA - 1
            JEL = ICMREF (IEL, IFA + 4)

            JFA = ICMREF (IEL, IFA + 8)
            JCBED = top_cell_no
            IF (JEL.GT.0) JCBED = NLYRBT (JEL, 1) - 1

            IF (BEXBK) JCBED = NHBED (IEL, IBK)
            QBK = ZERO
            DO 740 JCL = JCBED+1, top_cell_no
               QBK = QBK + QVSH (JFA, JCL, JEL)

740         END DO
! !!! mod.s to make definition of exchange flows consistent with balwat
! SPA, 04/11/98
            QBKF (IEL, IBK) = QBK
            QBKB (IEL, IBK) = QI * IBANK * WET

            QBKI (IEL, IBK) = QI * IBANK * (1 - WET)
760      END DO

780   END DO

   END SUBROUTINE VSSIM


END MODULE
