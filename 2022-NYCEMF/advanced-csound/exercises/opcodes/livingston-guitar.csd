<CsoundSynthesizer>
<CsOptions>
csound -RWfo dac:plughw:2,0
</CsOptions>
<CsInstruments>
;*******************************************
;Classical Guitar Physical model.
; Jeff Livingston 12/2000
;*******************************************
; The model takes pluck position, and pickup position (in % of string length), and generates
; a pluck excitation signal, representing the string displacement.  The pluck consists 
; of a forward and backward traveling displacement wave, which are recirculated thru two 
; separate delay lines, to simulate the one dimensional string waveguide, with 
; fixed ends.
; Losses due to internal friction of the string, and with air, as well as
; losses due to the mechanical impedance of the string terminations are simulated by 
; low pass filtering the signal inside the feedback loops.
; Delay line outputs at the bridge termination are summed and fed into an IIR filter
; modeled to simulate the lowest two vibrational modes (resonances) of the guitar body.
; The theory implies that force due to string displacement, which is equivalent to 
; displacement velocity times bridge mechanical impedance, is the input to the guitar
; body resonator model. Here we have modified the transfer fuction representing the bridge
; mech impedance, to become the string displacement to bridge input force transfer function.
; The output of the resulting filter represents the displacement of the guitar's top plate,
; and sound hole, since thier respective displacement with be propotional to input force.
; (based on a simplified model, viewing the top plate as a force driven spring).
;
; The effects of pluck hardness, and contact with frets during pluck release,
; have been modeled by injecting noise into the initial pluck, proportional to initial 
; string displacement.
;
; Note on pluck shape: Starting with a triangular displacment, I found a decent sounding
; initial pluck shape after some trial and error.  This pluck shape, which is a linear
; ramp, with steep fall off, doesn't necessarily agree with the pluck string models I've 
; studied.  I found that initial pluck shape significantly affects the realism of the 
; sound output, but I the treatment of this topic in musical acoustics literature seems
; rather limited as far as I've encountered.  
;******************************************** 
sr = 48000
ksmps =  1
nchnls = 2
0dbfs = 200000

;1D String Physical Model
instr 1
;inst st dur amp pch plksize(%) fbfac pickupPos(%) pluckPos(%)
; Initializations
  afwav   init 0
  abkwav  init 0
  abkdout init 0
  afwdout init 0 
  
  iEstr	 = 1/cpspch(6.04)
  ifqc   = cpspch(p5)
  idlt   = 1/(ifqc)		;note:delay time=2x length of string (time to traverse it)
  ipluck = .5*idlt * p6 * ifqc/cpspch(8.02)
  ifbfac = p7  			;feedback factor
  ibrightness = p10*exp(p6*log(2))/2 ;(exponentialy scaled) additive noise to add hi freq content
  ivibRate = p11	;vibrato rate
  ivibDepth pow 2,p12/12
  ivibDepth = idlt-1/(ivibDepth*ifqc)	;vibrato depth, +,- ivibDepth semitones
  ivibStDly = p13 ;vibrato start delay (secs)
  
  ;termination impedance model
  if0 = 10000 ;cutoff freq of LPF due to mech. impedance at the nut (2kHz-10kHz)
  iA0 = p7  ;damping parameter of nut impedance
  ialpha = cos(2*3.14159265*if0*1/sr)
  ia0 = .3*iA0/(2*(1-ialpha))  ; FIR LPF model of nut impedance,  H(z)=a0+a1z^-1+a0z^-2
  ia1 = iA0-2*ia0
  ;NOTE each filter pass adds a sampling period delay,so subtract 1/sr from tap time to compensate

  ;determine (in crude fashion) which string is being played
 ; icurStr = (ifqc > cpspch(6.04) ? 2 : 1)
 ; icurStr = (ifqc > cpspch(6.09) ? 3 : icurStr)
 ; icurStr = (ifqc > cpspch(7.02) ? 4 : icurStr)
 ; icurStr = (ifqc > cpspch(7.07) ? 5 : icurStr)
 ; icurStr = (ifqc > cpspch(7.11) ? 6 : icurStr)
 

  ipupos = p8*idlt/2; pick up position (in % of low E string length)
  ippos  = p9*idlt/2; pluck position (in % of low E string length)
  
  isegF = 1/sr
  isegF2 = ipluck
  iplkdelF = (ipluck/2 > ippos ? 0 : ippos-ipluck/2)
  isegB = 1/sr
  isegB2 = ipluck
  iplkdelB = (ipluck/2 > idlt/2-ippos ? 0 : idlt/2-ippos-ipluck/2)

;EXCITATION SIGNAL GENERATION
;the two excitation signals are fed into the fwd delay represent the 1st and 2nd 
; reflections off of the left boundary, and two accelerations fed into the bkwd delay 
; represent the the 1st and 2nd reflections off of the right boundary.
;Likewise for the backward traveling acceleration waves, only they encouter the 
; terminationsin the opposite order.
  ipw = 1;
  ipamp = p4*ipluck;4/ipluck

aenvstrf linseg 0,isegF,-ipamp/2,isegF2,0
  adel1	delayr idlt
  aenvstrf1 deltapi iplkdelF        ;initial forward traveling wave (pluck to bridge)
  aenvstrf2 deltapi iplkdelB+idlt/2 ;first forward traveling reflection (nut to bridge) 
	delayw aenvstrf

; inject noise for attack time string fret contact, and pre pluck vibrations against pick 
anoiz rand	ibrightness
aenvstrf1 = aenvstrf1 + anoiz*aenvstrf1
aenvstrf2 = aenvstrf2 + anoiz*aenvstrf2

; filter to account for losses along loop path
  aenvstrf2	filter2  aenvstrf2, 3, 0, ia0,ia1,ia0 
; combine into one signal (flip refl wave's phase)
  aenvstrf = aenvstrf1-aenvstrf2

; initial backward excitation wave  
  aenvstrb linseg 0,isegB,-ipamp/2,isegB2,0  
  adel2	delayr idlt
  aenvstrb1 deltapi iplkdelB        ; initial bdwd traveling wave (pluck to nut)
  aenvstrb2 deltapi idlt/2+iplkdelF ;first forward traveling reflection (nut to bridge) 
	delayw aenvstrb

; initial bdwd traveling wave (pluck to nut)
;  aenvstrb1	delay	aenvstrb,  iplkdelB
; first bkwd traveling reflection (bridge to nut)
;  aenvstrb2	delay	aenvstrb, idlt/2+iplkdelF
  
; inject noise
aenvstrb1 = aenvstrb1 + anoiz*aenvstrb1
aenvstrb2 = aenvstrb2 + anoiz*aenvstrb2
    
  
; filter to account for losses along loop path
  aenvstrb2	filter2  aenvstrb2, 3, 0, ia0,ia1,ia0
; combine into one signal (flip refl wave's phase)
  aenvstrb	=	aenvstrb1-aenvstrb2

; low pass to band limit initial accel signals to be < 1/2 the sampling freq
  ainputf  tone  aenvstrf,sr*.9/2
  ainputb  tone  aenvstrb,sr*.9/2
; additional lowpass filtering for pluck shaping\
;Note, it would be more efficient to combine stages into a single filter
  ainputf  tone  ainputf,sr*.9/2
  ainputb  tone  ainputb,sr*.9/2

; Vibrato generator
avib poscil ivibDepth, ivibRate,1
avibdl		delayr		ivibStDly*1.1+.001
avibrato	deltapi	ivibStDly
			delayw		avib

; Dual Delay line, 
; NOTE: delay length longer than needed by a bit so that the output at t=idlt will be interpolated properly        
; fwd 		delay line
  afd  		delayr (idlt+ivibDepth)*1.1		;forward traveling wave delay line
  afwav  	deltapi ipupos    	;output tap point for fwd traveling wave
  afwdout	deltapi idlt-1/sr + avibrato	;output at end of fwd delay (left string boundary)
  afwdout	filter2  afwdout, 3, 0, ia0,ia1,ia0  ;lpf/attn due to reflection impedance		
  			delayw  ainputf + afwdout*ifbfac*ifbfac

; bkwd delay line
  abkwd  	delayr (idlt+ivibDepth)*1.1          	;backward trav wave delay line
  abkwav  	deltapi idlt/2-ipupos		;output tap point for bkwd traveling wave
;  abkterm	deltapi	idlt/2				;output at the left boundary
  abkdout	deltapi idlt -1/sr + avibrato	;output at end of bkwd delay (right string boundary)
  abkdout	filter2  abkdout, 3, 0, ia0,ia1,ia0  	
  			delayw  ainputb + abkdout*ifbfac*ifbfac
;resonant body filter model, from Cuzzucoli and Lombardo
;IIR filter derived via bilinear transform method
;the theoretical resonances resulting from circuit model should be:
;resonance due to the air volume + soundhole = 110Hz (strongest)
;resonance due to the top plate = 220Hz
;resonance due to the inclusion of the back plate = 400Hz (weakest)
aresbod filter2 (afwdout + abkdout), 5,4, .000000000005398681501844749,.00000000000001421085471520200,-.00000000001076383426834582,-00000000000001110223024625157,.000000000005392353230604385,-3.990098622573566,5.974971737738533,-3.979630684599723,.9947612723736902
           asig =  1500*(afwav+abkwav+aresbod*.000000000000000000003);
		outs asig,asig
           endin
;************************************************************************
;1D String Physical Model
; slightly modified to be low string model: 
; More pluck shape smoothing via initial filtering.
; Increased resonant body filter gain
;
instr 2
;inst st dur amp pch plksize(%) fbfac pickupPos(%) pluckPos(%)
; Initializations
  afwav   init 0
  abkwav  init 0
  abkdout init 0
  afwdout init 0

  iEstr	 = 1/cpspch(6.04)
  ifqc   = cpspch(p5)
  idlt   = 1/(ifqc)		;note:delay time=2x length of string (time to traverse it)
  ; pluck length, scaled relative to a selected reference note, middle D
  ipluck = .008*p6;.5*idlt * p6 * ifqc/cpspch(8.02);
  ifbfac = p7  			;feedback factor
  ibrightness = p10*exp(p6*log(10))/10 ;(exponentialy scaled) additive noise 
  										;to add hi freqs for larger plucks
  ivibRate = p11	;vibrato rate
  ivibDepth pow 2,p12/12
  ivibDepth = idlt-1/(ivibDepth*ifqc)	;vibrato depth, +,- ivibDepth semitones
  ivibStDly = p13 ;vibrato start delay (secs)
  ;termination impedance model
  if0 = 6000 ;cutoff freq of LPF due to mech. impedance at the nut (2kHz-10kHz)
  iA0 = p7  ;damping parameter of nut impedance
  ialpha = cos(2*3.14159265*if0*1/sr)
  ia0 = .3*iA0/(2*(1-ialpha))  ; FIR LPF model of nut impedance,  H(z)=a0+a1z^-1+a0z^-2
  ia1 = iA0-2*ia0
  ;NOTE each filter pass adds a sampling period delay,so subtract 1/sr from tap time to compensate
  

  ipupos = .5*p8*idlt;iEstr	;pick up position (in % of low E string length)
  ippos  = .5*p9*idlt;iEstr	;pluck position (in % of low E string length)

;EXCITATION SIGNAL GENERATION
;acceleration impulse rise time width and peak ampl 
  ipw = 1;
  ipamp = p4*ipluck;4/ipluck
; initial excitation wave 
  aenvstri1 linseg 0,ipw/sr,ipamp/4,ipw/sr,0
  aenvstri2 linseg 0,2*ipw/sr,0,12*ipluck/16,-ipamp/2,ipluck/16,ipamp/4,ipw/sr,0
  aenvstri = aenvstri1+aenvstri2
; initial forward traveling wave (pluck to bridge)
  aenvstrf1	delay	aenvstri,ippos
;  first forward traveling reflection (nut to bridge)
  aenvstrf2 delay	aenvstri, abs(idlt-ippos-1/sr)

; inject noise for attack time string fret interaction, and pre pluck release vibrations (other natural nonlinearities?)
anoiz rand	ibrightness
aenvstrf1 = aenvstrf1 + anoiz*aenvstrf1
aenvstrf2 = aenvstrf2 + anoiz*aenvstrf2

; filter to account for losses along loop path
  aenvstrf2	filter2  aenvstrf2, 3, 0, ia0,ia1,ia0 
; combine into one signal (flip refl wave's phase)
  aenvstrf = aenvstrf1-aenvstrf2
  
; initial bdwd traveling wave (pluck to nut)
  aenvstrb1	delay	aenvstri, abs(idlt/2-ippos)
; first bkwd traveling reflection (bridge to nut)
  aenvstrb2	delay	aenvstri, abs(idlt/2+ippos-1/sr)
  
; inject noise
aenvstrb2 = aenvstrb2 + anoiz*aenvstrb2
aenvstrb1 = aenvstrb1 + anoiz*aenvstrb1
    
  
; filter to account for losses along loop path
  aenvstrb2	filter2  aenvstrb2, 3, 0, ia0,ia1,ia0
; combine into one signal (flip refl wave's phase)
  aenvstrb	=	aenvstrb1-aenvstrb2
; low pass to band limit initial accel signals to be < 1/2 the sampling freq
  ainputf  tone  aenvstrf,sr*.5/2
  ainputb  tone  aenvstrb,sr*.5/2
; additional lowpass filtering for pluck shaping\
;Note, it would be more efficient to combine stages into a single filter
  ainputf  tone  ainputf,sr*.5/2
  ainputb  tone  ainputb,sr*.5/2

; Vibrato generator
avib oscili ivibDepth, ivibRate,1
avibdl		delayr		ivibStDly*1.1+.001
avibrato	deltapi	ivibStDly
			delayw		avib

 
; Dual Delay line, 
; NOTE: delay length longer than needed by a bit so that the output at t=idlt will be interpolated properly        
  afd  		delayr (idlt+ivibDepth)*1.1             ;forward traveling wave delay line
  afwav  	deltapi ipupos      		;output tap point for fwd traveling wave
  afwdout	deltapi idlt -1/sr + avibrato	;output at end of fwd delay (left string boundary)
  afwdout	filter2  afwdout, 3, 0, ia0,ia1,ia0  ;lpf/attn due to reflection impedance		
  			delayw  ainputf + afwdout*ifbfac*ifbfac

; bkwd delay line
  abkwd  	delayr (idlt+ivibDepth)*1.1          	;backward trav wave delay line
  abkwav  	deltapi idlt/2-ipupos		;output tap point for bkwd traveling wave
;  abkterm	deltapi	idlt/2				;output at the left boundary
  abkdout	deltapi idlt -1/sr + avibrato	;output at end of bkwd delay (right string boundary)
  abkdout	filter2  abkdout, 3, 0, ia0,ia1,ia0  	
  			delayw  ainputb + abkdout*ifbfac*ifbfac
;resonant body filter model (coupling impedance via bridge), from Cuzzucoli and Lombardo
;IIR filter derived via bilinear transform method
aresbod filter2 (afwdout + abkdout), 5,4, .000000000005398681501844749,.00000000000001421085471520200,-.00000000001076383426834582,-00000000000001110223024625157,.000000000005392353230604385,-3.990098622573566,5.974971737738533,-3.979630684599723,.9947612723736902
           asig =  (300*(afwav+abkwav)+aresbod*.00000000000000000018);
	outs asig,asig
endin

</CsInstruments>
<CsScore>
f1	0	65537	10	1	;  	sine wave
;sco for phmtest35.orc
t00 60
;in	st	dur	amp	 pch	plklen	fbfac	pkuppos	plkpos	brt	vibRt	vibDpt	vibDly
i2	0	3.7	8000	6.04	.30	0.9960	.25	.21	1	0	0	0
i2   .005  .   8000 6.09  		
i2   .01   .   8000  7.02  	
i1	.015	3.7	6500	7.07	.65	0.9980	.0     .21	1	0	0	0
i1   .020  .   6500  7.11  .75    .          .         .34
i1  .025   .   6500  8.04   .85     0.9990     .0        .71		
s
i1    0   3.7  7500  8.04  .85     0.9990    .0      .71	1	0	0	0
i1   .1   .    <     7.11  <      <          <         .34						
i1   .2   .    7500  7.07  .65      0.9980   .0       .34
i2   .3   3.7  7000  7.02  .45     0.9960    .25       .21	1	0	0	0
i2   .4   .    <     6.09  <      <          <         .
i2   .5   .    7000  6.04  .45     0.9960    .25      .21	
s
t00 88
;vary pluck position
;in	st	dur	amp	 pch	plklen	fbfac	pkuppos	plkpos	brt	vibRt	vibDpt	vibDly
i02.1	0.25	0.75	9500	6.10	.40    0.9960	.0      .21	1	0	0	0		
i02.1	1	.		.		.		<		.		<		<										
i02.1	1.75	.		.		.		<		.		<		<													
i02.1	2.5	.		.		.		<		.		<		<														
i02.1	3.25	.		.		.		<		.		<		<														
i02.1	4	.		.		.		<		.		<		<														
i02.1	4.75	1.5		.		.		.40		.		.0		.89	
;in	st	dur	amp	 pch	plklen	fbfac	pkuppos	plkpos	brt	vibRt	vibDpt	vibDly
i01.1	6.25	0.75	9500	8.04	.65    0.9980	.0      .01	1	0	0	0	
i01.1	7	.	.	.	<	.	<	<										
i01.1	7.75	.	.	.	<	.	<	<													
i01.1	8.5	.	.	.	<	.	<	<														
i01.1	9.25	.	.	.	<	.	<	<														
i01.1	10	.	.	.	<	.	<	<														
i01.1	10.75	1.5	.	.	.55	.	.0	.99
s
;vary pickup position
t00 88
;in	st	dur	amp	 pch	plklen	fbfac	pkuppos	plkpos	brt	vibRt	vibDpt	vibDly
i02.1	0.25	0.75	9500	6.10	.40    0.9980	.01      .35	1	0	0	0	
i02.1	1	.	.	.	<	.	<	<										
i02.1	1.75	.	.	.	<	.	<	<													
i02.1	2.5	.	.	.	<	.	<	<														
i02.1	3.25	.	.	.	<	.	<	<														
i02.1	4	.	.	.	<	.	<	<														
i02.1	4.75	1.5	.	.	.40	.	.99	.35	
;in	st	dur	amp	 pch	plklen	fbfac	pkuppos	plkpos	brt	vibRt	vibDpt	vibDly
i01.1	6.25	0.75	9500	8.04	.65    0.9980	.01      .35	1	0	0	0
i01.1	7	.	.	.	<	.	<	<										
i01.1	7.75	.	.	.	<	.	<	<													
i01.1	8.5	.	.	.	<	.	<	<														
i01.1	9.25	.	.	.	<	.	<	<														
i01.1	10	.	.	.	<	.	<	<														
i01.1	10.75	1.5	.	.	.65	.	.99	.35
s
t00 88
;vary pluck length
;in     st      dur     amp     pch     plklen  fbfac   pkupPos pluckPos brightness
i02.1	0.25	0.75	6000	6.10	.10     0.9960	.00     .25		2	0	0	0	
i02.1	1	.	.	.	<	.	<	<		<							
i02.1	1.75	.	.	.	<	.	<	<		<										
i02.1	2.5	.	.	.	<	.	<	<		<											
i02.1	3.25	.	.	.	<	.	<	<		<											
i02.1	4	.	.	.	<	.	<	<		<											
i02.1	4.75	3.5	.	.	.999	.	.00	.25		2
;in     st      dur     amp     pch     plklen fbfac    pkupPos  pluckPos 	
i01.1	8.25	0.75	5500	8.04	.10    0.9980	.00     .84		5	0	0	0	
i01.1	9	.	.	.	<	.	<	<		<							
i01.1	9.75	.	.	.	<	.	<	<		<										
i01.1	10.5	.	.	.	<	.	<	<		<												
i01.1	11.25	.	.	.	<	.	<	<		<											
i01.1	12	.	.	.	<	.	<	<		<											
i01.1	12.75	4	.	.	.999	.	.00	.84		5
s
;E minor prelude
;voice 1
;in     st      dur     amp     pch     plklen fbfac    pkupPos pluckPos brightness
i02	0.0	1.5	9000	6.04	.65    0.9960   .22     .31		1	0	0	0
i02	1.5	.	.	.	.	.	.	.
i02	3.0	.	.	.	.	.	.	.
i02	4.5	.	.	.	.	.	.	.
i02	6.0	.	.	.	.	.	.	.
i02	7.5	.	.	.	.	.	.	.
i02	9.0	.	.	.	.	.	.	.
i02	10.5	.	.	.	.	.	.	.
i02	12.0	.	.	7.02	.	.	.	.20
i02	13.5	.	.	7.02	.	.	.	.
i02	15.0	.	.	7.00	.	.	.	. 
i02	16.5	.	.	6.09	.55	.	.	.
i02	18.0	.	.	6.11	.65	.	.	.
i02	19.5	.	.	6.11	.	.	.	.
i02	21.0	1.5	.	6.04	.65	.	.	.31
i02	22.5	1.5	.	7.04	.70	.	.	.22
;voice  2
;in     st      dur     amp     pch     plklen fbfac      pkupPos  pluckPos 	brightness
i01.1	0.25	0.75	5000	7.07	.75    0.9980     .0      .24		1	0	0	0	
i01.1	1	.	.	.												
i01.1	1.75	.	.	.												
i01.1	2.5	.	.	.													
i01.1	3.25	.	.	7.09													
i01.1	4	.	.	.													
i01.1	4.75	.	.	.													
i01.1	5.5	.	.	.													
i01.1	6.25	.	.	.													
i01.1	7	.	.	.													
i01.1	7.75	.	.	7.06													
i01.1	8.5	.	.	.													
i01.1	9.25	.	.	7.07													
i01.1	10	.	.	.													
i01.1	10.75	.	.	.													
i01.1	11.5	.	.	.													
i01.1	12.25	.	.	7.08													
i01.1	13	.	.	.													
i01.1	13.75	.	.	.													
i01.1	14.5	.	.	.													
i01.1	15.25	.	.	7.09													
i01.1	16	.	.	.													
i01.1	16.75	.	.	.													
i01.1	17.5	.	.	.													
i01.1	18.25	.	.	7.07													
i01.1	19	.	.	.													
i01.1	19.75	.	.	7.06													
i01.1	20.5	.	.	.													
i01.1	21.25	.	.	7.07													
i01.1	22	2		
;voice 3
;in     st      dur     amp     pch     plklen fbfac	pkupPos	pluckPos	brightness
i01.2	0.5	0.75	5000	7.11	.85    0.9975	.0	.25		1	0	0	0
i01.2	1.25	.	.	.												
i01.2	2	.	.	.												
i01.2	2.75	.	.	.													
i01.2	3.5	.	.	8.00													
i01.2	4.25	.	.	.													
i01.2	5	.	.	.													
i01.2	5.75	.	.	.													
i01.2	6.5	.	.	.													
i01.2	7.25	.	.	.													
i01.2	8	.	.	7.09													
i01.2	8.75	.	.	.													
i01.2	9.5	.	.	7.11													
i01.2	10.25	.	.	.													
i01.2	11	.	.	.													
i01.2	11.75	.	.	.													
i01.2	12.5	.	.	.													
i01.2	13.25	.	.	.													
i01.2	14	.	.	.													
i01.2	14.75	.	.	.													
i01.2	15.5	.	.	8.00													
i01.2	16.25	.	.	.													
i01.2	17	.	.	.													
i01.2	17.75	.	.	.													
i01.2	18.5	.	.	7.11													
i01.2	19.25	.	.	.													
i01.2	20	.	.	7.09													
i01.2	20.75	.	.	.													
i01.2	21.5	.	.	7.11													
i01.2	22.25	1.75	.	.	.	.	.	.
;voice 4
;in     st      dur     amp     pch     plklen 	fbfac	pkupPos	pluckPos 	brightness																	
i01.3	0.75	1.5	6600	8.07	.97    	0.9991	.00	.84 		1	0	0	0
i01.3	2.25	.	.	8.04	.94	.	.	.83										
i01.3	3.75	.	.	8.07	.97	.	.	.85										
i01.3	5.25	.	.	.	.	.	.	.81									
i01.3	6.75	.	.	.	.	.	.	.82									
i01.3	8.25	.	6600	8.03	.93	.	.	.841										
i01.3	9.75	.	.	8.04	.94									
i01.3	11.25	.	.	.												
i01.3	12.75	.	.	8.05	.95												
i01.3	14.25	.	.	8.04	.94											
i01.3	15.75	.	.	.											
i01.3	17.25	.	.	8.06	.96											
i01.3	18.75	.	.	8.04	.94											
i01.3	20.25	.	.	8.03	.93											
i01.3	21.75	1.75	.	8.04	.94											
i01.3	22.50	1.5	.	7.04	.84	.997	.0	.74		1
s
;E minor prelude (chordal)
;voice 1
;in     st      dur     amp     pch     plklen	fbfac   pkupPos pluckPos brightness
i02	0.0	1.5	5500	6.04	.45	0.9960  .22    	.31		1	0	0	0
i02	1.5	.	.	.	.	.	.	.
i02	3.0	.	.	.	.	.	.	.
i02	4.5	.	.	.	.	.	.	.
i02	6.0	.	.	.	.	.	.	.
i02	7.5	.	.	.	.	.	.	.
i02	9.0	.	.	.	.	.	.	.
i02	10.5	.	.	.	.	.	.	.
i02	12.0	.	.	7.02	.40	.	.	.20
i02	13.5	.	.	7.02	.	.	.	.
i02	15.0	.	.	7.00	.45	.	.	.
i02	16.5	.	5000	6.09	.	.	.	.
i02	18.0	.	.	6.11	.	.	.	.
i02	19.5	.	.	6.11	.	.	.	.
i02	21.0	1.5	.	6.04	.45	.	.	.31
i02	22.5	5	4000	7.04	.50	.	.	.22
;voice 2
;in     st      dur     amp     pch     plklen fbfac      pkupPos  pluckPos 	brightness
i01.1	0.001	1.5	4000	7.07	.75    0.9980     .0      .49			1	0	0	0	
i01.1	1.501	.	.	.												
i01.1	3.001	.	.	7.09													
i01.1	4.501	.	.	.													
i01.1	6.001	.	.	.													
i01.1	7.501	.	.	7.06													
i01.1	9.001	.	.	7.07													
i01.1	10.501	.	.	.													
i01.1	12.001	.	3800	7.08													
i01.1	13.501	.	.	.													
i01.1	15.001	.	4000	7.09													
i01.1	16.501	.	.	.													
i01.1	18.001	.	.	7.07													
i01.1	19.50	.	.	7.06													
i01.1	21.001	8.5	.	7.07													
;voice 3
;in     st      dur     amp     pch     plklen fbfac	pkupPos	pluckPos	brightness
i01.2	0.01	1.5	4500	7.11	.75    0.9980	.0	.49		1		0	0	0
i01.2	1.51	.	.	.												
i01.2	3.01	.	.	8.00													
i01.2	4.51	.	.	.													
i01.2	6.01	.	.	.													
i01.2	7.51	.	.	7.09													
i01.2	9.01	.	.	7.11													
i01.2	10.51	.	.	.													
i01.2	12.01	.	.	.													
i01.2	13.51	.	.	.													
i01.2	15.01	.	.	8.00													
i01.2	16.51	.	.	.													
i01.2	18.01	.	.	7.11													
i01.2	19.51	.	.	7.09													
i01.2	21.01	8.5	.	7.11												
;voice 4
;in     st      dur     amp     pch     plklen fbfac	pkupPos	pluckPos 	brightness																	
i01.3	0.015	1.5	4500	8.07	.85    0.9998	.0	.89		1	0	0	0
i01.3	1.515	.	.	8.04	.	.	.	.88										
i01.3	3.015	.	.	8.07	.75	.	.	.85		.95								
i01.3	4.515	.	.	.		.76	.	.		.9									
i01.3	6.015	.	.	.		.765	.	.		1								
i01.3	7.515	.	.	8.03	.77	.	.	.84										
i01.3	9.015	.	.	8.04											
i01.3	10.515	.	.	.												
i01.3	12.015	.	4500	8.05												
i01.3	13.515	.	.	8.04												
i01.3	15.015	.	4500	.											
i01.3	16.515	.	.	8.06												
i01.3	18.015	.	4200	8.04												
i01.3	19.515	.	4500	8.03												
i01.3	21.015	8.5	.	8.04												
i01.3	22.500	7	.	7.04	.	.997	


</CsScore>
</CsoundSynthesizer>
<bsbPanel>
 <label>Widgets</label>
 <objectName/>
 <x>643</x>
 <y>55</y>
 <width>703</width>
 <height>567</height>
 <visible>true</visible>
 <uuid/>
 <bgcolor mode="nobackground">
  <r>231</r>
  <g>46</g>
  <b>255</b>
 </bgcolor>
 <bsbObject version="2" type="BSBVSlider">
  <objectName>slider1</objectName>
  <x>5</x>
  <y>5</y>
  <width>20</width>
  <height>100</height>
  <uuid>{a86b95ba-8a1f-4285-8e45-ab5d6c6e6e32}</uuid>
  <visible>true</visible>
  <midichan>0</midichan>
  <midicc>-3</midicc>
  <minimum>0.00000000</minimum>
  <maximum>1.00000000</maximum>
  <value>0.00000000</value>
  <mode>lin</mode>
  <mouseControl act="jump">continuous</mouseControl>
  <resolution>-1.00000000</resolution>
  <randomizable group="0">false</randomizable>
 </bsbObject>
</bsbPanel>
<bsbPresets>
</bsbPresets>
