      PROGRAM DECAYLHE
C--------------- PREAMBLE: COMMON BLOCK DECLARATIONS ETC -------------
C...All real arithmetic done in double precision.
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
C...The PYTHIA event record:
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      SAVE /PYJETS/
C...Pythia parameters
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYDAT3/MDCY(500,3),MDME(8000,2),BRAT(8000),KFDP(8000,5)

C...EXTERNAL statement links PYDATA on most machines.
      CHARACTER*3 chlun
      EXTERNAL PYDATA

C-------------------------- PYTHIA SETUP -----------------------------

C...1) Open LHEF file on unit LUN, and tell Pythia where to find it.
      LUN=88
      OPEN(LUN,FILE='VBF_inv_LHEs/event_gf_inv_14000.lhe')
      WRITE(CHLUN,'(I3)') LUN
      CALL PYGIVE('MSTP(161)='//CHLUN)
      CALL PYGIVE('MSTP(162)='//CHLUN)

      CALL PYGIVE('MDME(214,1)=0')             !. Turing off the Higgs -> b bar decay
      CALL PYGIVE('MDME(210,1)=0') ! h0->d_dbar off. 
      CALL PYGIVE('MDME(211,1)=0') ! h0->u_ubar off. 
      CALL PYGIVE('MDME(212,1)=0') ! h0->s_sbar off. 
      CALL PYGIVE('MDME(213,1)=0') ! h0->c_cbar off. 
      CALL PYGIVE('MDME(214,1)=0') ! h0->b_bbar off.
      CALL PYGIVE('MDME(215,1)=0') ! h0->t_tbar off.
      CALL PYGIVE('MDME(218,1)=0') ! h0->e+e- off. 
      CALL PYGIVE('MDME(219,1)=0') ! h0->mu+mu- off. 
      CALL PYGIVE('MDME(220,1)=0') ! h0->tau+tau- off.
      CALL PYGIVE('MDME(222,1)=0') ! h0->gg off.
      CALL PYGIVE('MDME(224,1)=0') ! h0->gamma_Z0 off. 
      CALL PYGIVE('MDME(225,1)=1') ! h0->Z0_Z0 ON. 
      CALL PYGIVE('MDME(226,1)=0') ! h0->W+W- off.
      CALL PYGIVE('MDME(223,1)=0') ! h0->gamma_gamma off.

      CALL PYGIVE('MDME(174,1)=0') ! Z0->d_dbar off.
      CALL PYGIVE('MDME(175,1)=0') ! Z0->u_ubar off.
      CALL PYGIVE('MDME(176,1)=0') ! Z0->s_sbar off.
      CALL PYGIVE('MDME(177,1)=0') ! Z0->c_cbar off.
      CALL PYGIVE('MDME(178,1)=0') ! Z0->b_bar off.
      CALL PYGIVE('MDME(179,1)=0') ! Z0->t_tbar off.
      CALL PYGIVE('MDME(182,1)=0') ! Z0->e+e- off.
      CALL PYGIVE('MDME(183,1)=1') ! Z0->nu_e_nu_ebar ON.
      CALL PYGIVE('MDME(184,1)=0') ! Z0->mu_mu+ off.
      CALL PYGIVE('MDME(185,1)=1') ! Z0->nu_mu_nu_mubar ON.
      CALL PYGIVE('MDME(186,1)=0') ! Z0->tau-tau+ off.
      CALL PYGIVE('MDME(187,1)=1') ! Z0->nu_tau_nu_taubar ON.

C...2) Initialize Pythia for user process  
      CALL PYINIT('USER',' ',' ',0D0)

C------------------------- GENERATE EVENTS ---------------------------

C...Initial values for number of events and cumulative charged multiplicity
      IEV=0
      DNSUM=0D0
      DN2SUM=0D0

C...Get next event from file and process it
 100    CALL PYEVNT

C...If event generation failed, quit loop
      IF(MSTI(51).EQ.1) THEN
        GOTO 999
      ENDIF

C...Else count up number of generated events
      IEV=IEV+1

C...Print first event, both LHEF input and Pythia output.
      IF(IEV.LE.1) THEN   !. Event 1
        CALL PYLIST(7)
c        CALL PYLIST(2)
      ENDIF

C.../PYJETS/ now contains a fully generated event.
C...Insert user analysis here (or save event to output) 
C...(example: count charged multiplicity)


      MSTP(111)=1    !. Should allow hadronisation 
      MSTP(61)=1
      MSTP(71)=1
      MSTP(161)=1
      MSTP(164)=1
      MSTP(81)=20    !. Multiple interactions


C...Loop back to look for next event
      GOTO 100

C...Jump point when end-of-file reached (or other problem encountered)
C...Print final statistics.
 999    CALL PYSTAT(2)


c      DNAVG=DNSUM/IEV
c      DNRMS=SQRT(DN2SUM/IEV)
c      SIGMA2=MAX(0D0,DNRMS**2-DNAVG**2)
c     WRITE(*,'(1x,A,1x,F6.1,5x,A,1x,F6.1)') 
c   &    'Invisible Higgs samples decayed, please see above for event listing <Nch> =', 
c  &    DNAVG, '+/-',SQRT(SIGMA2/IEV)
c  PRINT*
c
        PRINT *, "Samples decayed, please see above for event listing"
      END
