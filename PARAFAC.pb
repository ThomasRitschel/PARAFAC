; fluorescence PARAFAC


InitKeyboard()
InitSprite()
UsePNGImageEncoder()
IncludeFile "matrix.pb"

;{ Init
Structure xy ;Paar Structure für alle möglichen Wertepaar-Felder
  x.d
  y.d
EndStructure

Structure eventstructure
  event.l
  eventtype.l
  eventgadget.l
EndStructure

Global n_student_t.i=0
Global Dim student_t_verteilung.xy(n_student_t)
Global maindirectory.s=GetCurrentDirectory()
Global symbolfont.i=LoadFont(#PB_Any, "Symbol", 16)
Global FPU_ControlWord.w
!fstcw [v_FPU_ControlWord]

Global squaresum.d
Global lackoffit.d
Global grayscale.b
Global dev_rmse.d
Global exoffset.l
Global emoffset.l
Global totvar.d
Global mean.d
Global localmean.d
Global localrange.d
Global explainedv.d
Global calccore.b
Global corecons.d
Global breite.l
Global scaleplot_x.d=1
Global scaleplot_y.d=1
Global R.d=1
Global stufen.i=6
Global calculationtime.l
Global scalex_em.d
Global scalex_ex.d
Global scaley_spectra.d
Global middle.d
Global iteration.l=0
Global n_samp.l=-1
Global correct.b=0
Global absorbance.b=0
Global drawnumber.i=-1
Global rmse.d=0
Global ratio.d=1
Global old_ssq.d=1
Global ex_start.d
Global em_start.d
Global em_end.d
Global ex_end.d
Global nonneg_alt.b=1
Global nonneg.b=1
Global n_em.l
Global n_ex.l
Global n_comp.l=6
Global n_comp_alt.l
Global n_fixed.i=-1
Global n_fixed_alt.i=-1
Global scaledev.d=1
Global n_events.l=0
Global generic_names.s
Global Dim files.s(0)
Global Dim events.eventstructure(n_events)

Global scalerange_em.d
Global startscale_em.d
Global endscale_em.d
Global n_ticks_em.i
Global scalerange_ex.d
Global startscale_ex.d
Global endscale_ex.d
Global n_ticks_ex.i
Global ssq.d=0
Global nssq.d=0
Global cvssq.d=0
Global nrmse.d=0
Global cvrmse.d=0
Global range.d=0
Global interval.d=1
Global interval_z.d=1
Global decimals.l=0
Global decimals_z.l=0
Global twoD_comps.b=1
Global kindofdev.b
Global mini.b=0
Global generic.b=0
Global separator.s

Global directory.s
starttime.l=ElapsedMilliseconds()

OpenPreferences("init.dat")
directory=ReadPreferenceString("PARAFAC_defaultdirectory",GetCurrentDirectory())
interval=ReadPreferenceDouble("PARAFAC_interval",1)
interval_z=ReadPreferenceDouble("PARAFAC_interval_z",1)
decimals=ReadPreferenceInteger("PARAFAC_decimals",0)
decimals_z=ReadPreferenceInteger("PARAFAC_decimals_z",0)
scaleplot_x=ReadPreferenceDouble("PARAFAC_plotscale_x",1)
scaleplot_y=ReadPreferenceDouble("PARAFAC_plotscale_y",1)
scaledev=ReadPreferenceDouble("PARAFAC_devscale",1)
kindofdev=ReadPreferenceInteger("PARAFAC_kindofdev",0)
em_start=ReadPreferenceInteger("PARAFAC_em_start",230)
ex_start=ReadPreferenceInteger("PARAFAC_ex_start",230)
em_end=ReadPreferenceInteger("PARAFAC_em_end",700)
ex_end=ReadPreferenceInteger("PARAFAC_ex_end",500)
correct=ReadPreferenceInteger("PARAFAC_correct",1)
mini=ReadPreferenceInteger("PARAFAC_minifiles",0)
absorbance=ReadPreferenceInteger("PARAFAC_absorbance",1)
generic=ReadPreferenceInteger("PARAFAC_generic",0)
comp_pos=ReadPreferenceInteger("PARAFAC_component_position",1)

ClosePreferences()
Declare export_file()

#eol=Chr(13)+Chr(10)
Enumeration
  #Window_0
  #Window_1
  #Window_plot
  #Spin_ex_start
  #Spin_em_start
  #Spin_ex_end
  #Spin_em_end
  #Text_ex_start
  #Text_em_start
  #Button_import_samples
  #Spin_n_comp
  #Text_n_comp
  #Text_deviation
  #Text_ex_end
  #Text_em_end
  #String_deviation
  #Button_Start_PARAFAC
  #Button_Beenden
  #Button_Reset
  #Button_Import
  #Button_Pause
  #Button_importfixed
  #Checkbox_RMSE
  #Checkbox_correct
  #Checkbox_correctabs
  #Checkbox_mini_files
  #Checkbox_nonnegative
  #Checkbox_contour
  #CheckBox_relative_contents
  #CheckBox_component_position
  #Checkbox_2D_comps
  #Checkbox_generic
  #ListGadget
  #Checkbox_grayscale
  #Checkbox_Core
  #String_interval
  #Spin_decimals
  #Text_interval
  #Text_decimals
  #Spin_decimals_z
  #Text_decimals_z
  #Text_interval_z
  #Text_generic_start
  #Text_generic_end
  #String_interval_z
  #Text_scalex
  #String_scalex
  #Text_scaley
  #String_scaley
  #Button_About
  #Option_sesdci1
  #Option_sesdci2
  #Option_sesdci3
  
  
EndEnumeration
;}

Procedure Open_Window_0()
  If OpenWindow(#Window_0, 392, 134, 414, 146, "PARAFAC - Fluorescence Edition",  #PB_Window_SystemMenu | #PB_Window_SizeGadget | #PB_Window_TitleBar )
    CheckBoxGadget(#Checkbox_correct,10,10,150,20,"correct device specific")
    CheckBoxGadget(#Checkbox_correctabs,10,30,150,20,"correct for UV absorbance")
    ;CheckBoxGadget(#Checkbox_mini_files,170,30,300,20,"mini files")
    CheckBoxGadget(#Checkbox_generic,170,50,300,20,"generic files")
    SetGadgetState(#Checkbox_correct,correct)
    SetGadgetState(#Checkbox_correctabs,absorbance)
    ;SetGadgetState(#Checkbox_mini_files,mini)
    SetGadgetState(#Checkbox_generic,generic)
    SpinGadget(#Spin_ex_start, 110, 70, 80, 20,0, 1100000, #PB_Spin_Numeric)
    SetGadgetState(#Spin_ex_start,ex_start)      
    SpinGadget(#Spin_em_start, 110, 90, 80, 20, 0, 1100000, #PB_Spin_Numeric)
    SetGadgetState(#Spin_em_start,em_start)
    SpinGadget(#Spin_ex_end, 310, 70, 80, 20, 0, 1100000, #PB_Spin_Numeric)
    SetGadgetState(#Spin_ex_end,ex_end)      
    SpinGadget(#Spin_em_end, 310, 90, 80, 20, 0, 1100000, #PB_Spin_Numeric)
    SetGadgetState(#Spin_em_end,em_end)
    ;SpinGadget(#Spin_ex_start, 180, 40, 100, 20, 0, 5000, #PB_Spin_Numeric)
    ;SetGadgetState(#Spin_ex_start,ex_start)      
    ;SpinGadget(#Spin_em_start, 180, 70, 100, 20, 0, 5000, #PB_Spin_Numeric)
    ;SetGadgetState(#Spin_em_start,n_ex)
    TextGadget(#Text_ex_start, 10, 73, 100, 20, "Excitation Start [nm]")
   ; TextGadget(#Text_generic_start, 10, 73, 100, 20, "Generic Start")
    TextGadget(#Text_em_start, 10, 93, 100, 20, "Emission Start [nm]")
    TextGadget(#Text_ex_end, 210, 73, 100, 20, "Excitation End [nm]")
    ;TextGadget(#Text_generic_end, 210, 73, 100, 20, "Generic End")
    TextGadget(#Text_em_end, 210, 93, 100, 20, "Emission End [nm]")
    ButtonGadget(#Button_import_samples, 80, 120, 130, 20, "Import Sample Files")      
  EndIf
EndProcedure

Procedure Open_Window_1()
  If OpenWindow(#Window_1, 390, 596, 606, 214, "PARAFAC - Fluorescence Edition",  #PB_Window_SystemMenu | #PB_Window_SizeGadget | #PB_Window_TitleBar )
    TextGadget(#Text_n_comp, 20, 13, 70, 20, "#components")
    SpinGadget(#Spin_n_comp, 90, 10, 50, 20, 1, 5000,#PB_Spin_Numeric)
    TextGadget(#Text_deviation,20,73,60,20,"devscale")
    StringGadget(#String_deviation,90,70,50,20,StrD(scaledev))
    TextGadget(#Text_scalex, 20, 33, 70, 20, "plotscale_x")
    StringGadget(#String_scalex,90,30,50,20,StrD(scaleplot_x))
    TextGadget(#Text_scaley, 20, 53, 70, 20, "plotscale y")
    StringGadget(#String_scaley,90,50,50,20,StrD(scaleplot_y))
    ;SpinGadget(#Spin_scale, 100, 30, 50, 20, 1, 5000,#PB_Spin_Numeric)
    ;SetGadgetState(#Spin_scale,scaleplot)
    SetGadgetState(#Spin_n_comp,n_comp)
    TextGadget(#Text_interval,160,13,60,20,"interval")
    StringGadget(#String_interval,230,10,50,20,StrD(interval))
    TextGadget(#Text_interval_z,160,33,60,20,"interval z")
    StringGadget(#String_interval_z,230,30,50,20,StrD(interval_z))
    TextGadget(#Text_decimals,160,53,60,20,"decimals")
    SpinGadget(#Spin_decimals, 230, 50, 50, 20, 0, 5000,#PB_Spin_Numeric)
    SetGadgetState(#Spin_decimals,decimals)
    TextGadget(#Text_decimals_z,160,73,60,20,"decimals z")
    SpinGadget(#Spin_decimals_z, 230, 70, 50, 20, 0, 5000,#PB_Spin_Numeric)
    SetGadgetState(#Spin_decimals_z,decimals_z)
    
    ;SpinGadget(#Spin_n_comp, 140, 10, 50, 20, 1, 5000,#PB_Spin_Numeric)
    ;TextGadget(#Text_deviation,30,38,100,20,"dev. scale")
   ; StringGadget(#String_deviation,140,35,50,20,"1")
    ;SetGadgetState(#Spin_n_comp,n_comp)
    ;TrackBarGadget(#Track_1, 30, 65, 240, 20, 0, 2000)
    ;SetGadgetState(#Track_1,Round(R*1000,#PB_Round_Nearest))
    ;TextGadget(#Text_2,270,68,40,20,StrD(R,2))
    ;TextGadget(#Text_n_comp, 30, 13, 110, 20, "Anzahl Komponenten")
    CheckBoxGadget(#Checkbox_RMSE, 20,110,100,20,"show recon")
    ButtonGadget(#Button_Start_PARAFAC, 140, 130, 100, 20, "PARAFAC")
    ButtonGadget(#Button_About, 260, 130, 50, 60, "About...")
    ButtonGadget(#Button_Reset, 20, 150, 100, 20, "Reset Loadings")
    ButtonGadget(#Button_Import, 20, 170, 100, 20, "Import State")
    ButtonGadget(#Button_importfixed, 20, 190, 100, 20, "Import Fixed Comp.")
    ButtonGadget(#Button_Pause, 140, 150, 100, 20, "Pause",#PB_Button_Toggle)
    ButtonGadget(#Button_Beenden, 140, 170, 100, 20, "Quit")  
    CheckBoxGadget(#Checkbox_nonnegative, 140,90,100,20,"nonnegative")
    CheckBoxGadget(#Checkbox_Core, 240,90,60,20,"Core")
    CheckBoxGadget(#Checkbox_2D_comps, 240,110,70,20,"2D-comps.")
    CheckBoxGadget(#CheckBox_relative_contents, 20,130,100,20,"relative contents")
    CheckBoxGadget(#CheckBox_component_position, 140,110,100,20,"comp. position")
    CheckBoxGadget(#Checkbox_contour, 330,170,70,20,"contour")
    CheckBoxGadget(#Checkbox_grayscale, 400, 170, 60, 20, "grayscale")
    SetGadgetState(#Checkbox_nonnegative,1)
    SetGadgetState(#CheckBox_component_position,comp_pos)
    ListViewGadget(#ListGadget, 330,10, 250, 160 )
    AddGadgetItem(#ListGadget, -1, "component spectra")
    AddGadgetItem(#ListGadget, -1, "component concentration")
    For i=0 To n_samp
      AddGadgetItem(#ListGadget, -1, files(i))
    Next i
    SetGadgetState(#ListGadget,0)
    OptionGadget(#Option_sesdci1,30,90,30,20,"ci")
    OptionGadget(#Option_sesdci2,60,90,30,20,"se")
    OptionGadget(#Option_sesdci3,90,90,30,20,"sd")
    SetGadgetState(#Option_sesdci1+kindofdev,1)
  EndIf
EndProcedure

Procedure.s get_separator(searchstring.s)
  result.s
    If CountString(searchstring,",")>CountString(searchstring,";") And CountString(searchstring,",")>CountString(searchstring,Chr(9)):result=",":EndIf
    If CountString(searchstring,";")>CountString(searchstring,",") And CountString(searchstring,";")>CountString(searchstring,Chr(9)):result=";":EndIf
    If CountString(searchstring,Chr(9))>CountString(searchstring,";") And CountString(searchstring,Chr(9))>CountString(searchstring,","):result=Chr(9):EndIf
  ProcedureReturn result
EndProcedure  

Procedure import_file_generic()
  
  tempf.s
  temp.d
  
  tempf=OpenFileRequester("Select Sample File",directory+"*.*","Alle Dateien (*.*)|*.*|CSV (*.csv)|*.csv|TXT (*.txt)|*.txt",0,#PB_Requester_MultiSelection)
  If tempf
    directory=GetPathPart(tempf)
  Else  
    End
  EndIf  
  
  Repeat  
    n_samp+1
    ReDim files.s(n_samp)
    files(n_samp)=tempf
    tempf=NextSelectedFileName()    
  Until tempf=""
  
  ;tempf=files(0)
  ;For i=0 To n_samp-1
  ;  files(i)=files(i+1)
  ;Next i
  ;files(n_samp)=tempf
       
  k=0
  filefound=0
  Repeat    
  If ReadFile(0,files(k))
    filefound=1
    CloseFile(0)
  EndIf 
  k+1
  Until k>n_samp Or filefound  
  
  If filefound
  ReadFile(0,files(k-1))
  tempf=ReadString(0)
  separator=get_separator(tempf)

  n_ex=CountString(tempf,separator)-1
  ;i=-1
  ;If ex_start<ValD(StringField(tempf,2,separator)):ex_start=ValD(StringField(tempf,2,separator)):EndIf
  ;If ex_end>ValD(StringField(tempf,n_ex+2,separator)):ex_end=ValD(StringField(tempf,n_ex+2,separator)):EndIf
;   Repeat
;     i+1
;   Until i>n_ex Or ValD(StringField(tempf,i+1,separator))>ex_start
;   ex_start_file=i
;   Repeat
;     i+1
;   Until i>n_ex Or ValD(StringField(tempf,i+1,separator))>ex_end
;  ex_end_file=i-1
 ; n_ex=ex_end_file-ex_start_file
  n_em=-1
  Repeat
    tempf=ReadString(0)
    n_em+1
  Until Eof(0) Or tempf=""   
  CloseFile(0); HIIIEIIEIIRIEIIRIERE
EndIf

Global Dim X.d(n_samp,n_em,n_ex)
Global Dim missing.d(n_em,n_ex)
  Global Dim blank.d(0,n_em,n_ex)
  Global Dim E.d(n_samp,n_em,n_ex)
  Global Dim X_recon.d(n_samp,n_em,n_ex)
  ;Global Dim best_A.d(n_samp,n_comp)
  ;Global Dim last_A.d(n_samp,n_comp)
  Global Dim A.d(n_samp,n_comp)
  Global Dim A_dev.d(n_samp,n_comp)
  Global Dim A_se.d(n_samp,n_comp)
  Global Dim A_sd.d(n_samp,n_comp)
  ;Global Dim last_B.d(n_em,n_comp)
  ;Global Dim last_C.d(n_ex,n_comp)
  Global Dim B.d(n_em,n_comp)
  Global Dim B_dev.d(n_em,n_comp)
  Global Dim C_dev.d(n_ex,n_comp)
  Global Dim B_se.d(n_em,n_comp)
  Global Dim C_se.d(n_ex,n_comp)
  Global Dim B_sd.d(n_em,n_comp)
  Global Dim C_sd.d(n_ex,n_comp)
  Global Dim C.d(n_ex,n_comp)
  Global Dim X_origin.d(n_samp,n_em,n_ex)
  Global Dim HIX.d(n_samp)
  Global Dim core.d(n_comp,n_comp,n_comp)
  Global Dim fixed.i(n_comp)
  Global Dim scale_em.d(n_em)
  Global Dim scale_ex.d(n_ex)
  Global Dim comps.d(n_em,n_ex,n_comp)
  ;Global Dim best_B.d(n_em,n_comp)
  ;Global Dim best_C.d(n_ex,n_comp)
  ;Global Dim best_A_dev.d(n_samp,n_comp)
  ;Global Dim best_B_dev.d(n_em,n_comp)
  ;Global Dim best_C_dev.d(n_ex,n_comp)
  
  For k=0 To n_samp  
    
    If ReadFile(0,files(k))
      filefound=1
    ElseIf files(k)
      filefound=0
      MessageRequester("Error","Could not open file")
    Else
      filefound=0
    EndIf   
    
    If filefound
      tempf=ReadString(0)      
      For i=0 To n_ex
        scale_ex(i)=ValD(StringField(tempf,2+n_ex-i,separator))
      Next i
      tempf=ReadString(0)
      For i=0 To n_em
        For j=0 To n_ex
          X(k,i,j)=ValD(StringField(tempf,2+n_ex-j,separator))          
        Next j
        scale_em(i)=ValD(StringField(tempf,1,separator))
        tempf=ReadString(0)
      Next i
      CloseFile(0)
    EndIf
    
  Next k
  
  


For i=0 To n_samp
  files(i)=RemoveString(GetFilePart(files(i)),".csv")
Next i  

em_start.d=scale_em(0)
ex_start.d=scale_ex(0)
em_step.d=(scale_em(n_em)-scale_em(0))/n_em
ex_step.d=(scale_ex(n_ex)-scale_ex(0))/n_ex
   width_first_order_rayleigh.i=30
   width_first_order_raman.i=9
   width_second_order_rayleigh.i=40
   width_second_order_raman.i=12

   For i=0 To n_em     
     lambda_em.d=i*em_step+em_start
        For j=0 To n_ex             
          lambda_ex.d=j*ex_step+ex_start  
          raman.d=10000000/(10000000/lambda_ex-3400)   
          ;If Abs(lambda_em-lambda_ex)<width_first_order_rayleigh Or Abs(lambda_em-2*lambda_ex)<width_second_order_rayleigh Or Abs(lambda_em-raman)<width_first_order_raman Or Abs(lambda_em-2*raman)<width_second_order_raman Or (lambda_em<lambda_ex And lambda_ex-lambda_em<40)
             If Abs(lambda_em-lambda_ex)<width_first_order_rayleigh Or Abs(lambda_em-2*lambda_ex)<width_second_order_rayleigh 
            
            missing(i,j)=1
          Else 
            missing(i,j)=0  
          EndIf
        Next j
      Next i
      
   For k=0 To n_samp     
     For i=0 To n_em
       lambda_em.d=i*em_step+em_start
        For j=0 To n_ex
          lambda_ex.d=j*ex_step+ex_start  
          If lambda_em<lambda_ex
            X(k,i,j)=0
          EndIf      
          If lambda_em>2*lambda_ex
            X(k,i,j)=0
          EndIf    
        Next j

      Next i    
  Next k
      
      
   
      
    
    CopyArray(X(),X_origin())
    
squaresum=0
mean=0
count=0
mean=0

max.d=-Pow(10,30)
min.d=Pow(10,30)
For i=0 To n_samp
  For j=0 To n_em
    For k=0 To n_ex
      If Not missing(j,k)
        If X(i,j,k)<min:min=X(i,j,k):EndIf
        If X(i,j,k)>max:max=X(i,j,k):EndIf
        mean+X(i,j,k)
        squaresum+Pow(X(i,j,k),2)
        count+1
      EndIf  
    Next k
  Next j
Next i
mean=mean/count
range=max-min

totvar=0
For i=0 To n_samp
  For j=0 To n_em
    For k=0 To n_ex
       If Not missing(j,k)
         totvar+Pow(X(i,j,k)-mean,2)
       EndIf  
    Next k
  Next j
Next i

    
    CallDebugger
    
EndProcedure

Procedure Import_file()  
  
  tempf.s
  temp.d
  
  tempf=OpenFileRequester("Select Sample Files",directory+"*.*","CSV (*.csv)|*.csv|Alle Dateien (*.*)|*.*",0,#PB_Requester_MultiSelection)
  If tempf
    directory=GetPathPart(tempf)
  Else 
    MessageRequester("Info","Files could not be opened")
    End
  EndIf  
  
  n_files-1
  Dim filenames.s(0)
  Repeat  
    n_files+1
    ReDim filenames.s(n_files)
    filenames(n_files)=tempf
    tempf=NextSelectedFileName()    
  Until tempf=""
  
  ReadFile(0,filenames(0))
  tempf=ReadString(0)  
  tempf=ReadString(0)

  ex_step.l=Round(ValD(Right(StringField(tempf,3,","),6)),#PB_Round_Nearest)-Round(ValD(Right(StringField(tempf,2,","),6)),#PB_Round_Nearest)

  n_underscores.i=CountString(StringField(tempf,2,","),"_")
  If StringField(StringField(tempf,2,","),n_underscores,"_")="EX"
    ex_start_file.l=Round(ValD(Right(StringField(tempf,2,","),6)),#PB_Round_Nearest)
    ex_step.l=Round(ValD(Right(StringField(tempf,3,","),6)),#PB_Round_Nearest)-Round(ValD(Right(StringField(tempf,2,","),6)),#PB_Round_Nearest)
    ex_end_file.l=Round(ValD(Right(StringField(tempf,CountString(tempf,",")+1,","),6)),#PB_Round_Nearest)
  Else
    starting_number.s=StringField(StringField(tempf,2,","),n_underscores+1,"_")
    ex_start_file.l=Round(ValD(StringField(StringField(tempf,2,","),n_underscores,"_")),#PB_Round_Nearest)
    ex_step.l=Round(ValD(StringField(StringField(tempf,3,","),n_underscores,"_")),#PB_Round_Nearest)-Round(ValD(StringField(StringField(tempf,2,","),n_underscores,"_")),#PB_Round_Nearest)
    k=0
    count.i=0
    Repeat
      k+1  
      If StringField(StringField(tempf,2+k,","),n_underscores+1,"_")=starting_number:count+1:EndIf
    Until k>CountString(tempf,",")
    ex_end_file.l=ex_start_file+ex_step*count
  EndIf
  
  tempf=ReadString(0)
  tempf=ReadString(0)
  em_start_file=Round(ValD(StringField(tempf,1,",")),#PB_Round_Nearest)
  tempf=ReadString(0)
  em_step.l=Round(ValD(StringField(tempf,1,",")),#PB_Round_Nearest)-em_start_file
  Repeat    
    em_end_file=Round(ValD(StringField(tempf,1,",")),#PB_Round_Nearest)
    tempf=ReadString(0)
  Until tempf=""
   
  If ex_start_file>ex_start:ex_start=ex_start_file
    MessageRequester("Warning","Specified beginning of excitation range smaller than data"+#eol+"Increasing to "+Str(ex_start)+"nm")
  EndIf
  If ex_end_file<ex_end:ex_end=ex_end_file
    MessageRequester("Warning","Specified end of excitation range greater than data"+#eol+"Decreasing to "+Str(ex_end)+"nm")
  EndIf
  If em_start_file>em_start:em_start=em_start_file
    MessageRequester("Warning","Specified beginning of emission range smaller than data"+#eol+"Increasing to "+Str(em_start)+"nm")
  EndIf
  If em_end_file<em_end:em_end=em_end_file
    MessageRequester("Warning","Specified end of emission range greater than data"+#eol+"Decreasing to "+Str(em_end)+"nm")
  EndIf
  If correct
    Dim excorr.d(380)
    Dim emcorr.d(380)
    If ReadFile(1,"excorr.txt")
      filefound=1
    Else
      filefound=0
      MessageRequester("Error","Could not open ex-corr file") 
    EndIf
    If filefound
      For i=0 To 380
        excorr(i)=ValD(ReadString(1))
      Next i  
    EndIf
    CloseFile(1)
    If ReadFile(1,"emcorr.txt")
      filefound=1
    Else
      filefound=0
      MessageRequester("Error","Could not open em-corr file") 
    EndIf
    If filefound
      For i=0 To 380
        emcorr(i)=ValD(ReadString(1))
      Next i  
    EndIf
    CloseFile(1)
    exoffset=ex_start-220
    emoffset=em_start-220
    If em_start<220:em_start=220:emoffset=0:EndIf
    If ex_start<220:ex_start=220:exoffset=0:EndIf
    If em_end>600:em_end=600:EndIf
    If ex_end>600:ex_end=600:EndIf
  EndIf  
  
  n_ex=(ex_end-ex_start)/ex_step
  n_ex_file=(ex_end_file-ex_start_file)/ex_step
  n_em=(em_end-em_start)/em_step
  
  CloseFile(0)
  
  n_samp=-1
    For l=0 To n_files ;add files to sample number
    ReadFile(0,filenames(l))
      tempf=ReadString(0)
      tempf=ReadString(0)
      additional_samples=(CountString(tempf,",")-1)/(n_ex_file+1)+1
      n_samp+additional_samples
      ReDim files.s(n_samp)
      If additional_samples>1
        For i=1 To additional_samples
          files(n_samp-additional_samples+i)=GetFilePart(filenames(l))+"_"+Str(i)
        Next i  
      Else
        files(n_samp)=GetFilePart(filenames(l))
      EndIf
    CloseFile(0)
  Next l 
  
  
  Global Dim X.d(n_samp,n_em,n_ex)
  Global Dim X_origin.d(n_samp,n_em,n_ex)
  Global Dim missing.d(n_em,n_ex)
  Global Dim scale_em.d(n_em)
  Global Dim scale_ex.d(n_ex)
  
   For i=0 To n_ex
     scale_ex(i)=ex_start+i*ex_step
   Next i
      
   For i=0 To n_em
     scale_em(i)=em_start+i*em_step
   Next i
   
   width_first_order_rayleigh.i=11
   width_first_order_raman.i=9
   width_second_order_rayleigh.i=15
   width_second_order_raman.i=12
   
   For i=0 To n_em     
     lambda_em.i=i*em_step+em_start
        For j=0 To n_ex             
          lambda_ex.i=j*ex_step+ex_start  
          raman.d=10000000/(10000000/lambda_ex-3400)   
          If Abs(lambda_em-lambda_ex)<width_first_order_rayleigh Or Abs(lambda_em-2*lambda_ex)<width_second_order_rayleigh Or Abs(lambda_em-raman)<width_first_order_raman Or Abs(lambda_em-2*raman)<width_second_order_raman
            missing(i,j)=1
          Else 
            missing(i,j)=0  
          EndIf
        Next j
    Next i
    
    skip=0
    For l=0 To n_files ;add files to sample number
      ReadFile(0,filenames(l))
      tempf=ReadString(0)
      tempf=ReadString(0)
      n_samp_file=(CountString(tempf,",")-1)/(n_ex_file+1)      
      ex_diff.l=(ex_start-ex_start_file)/ex_step
      Repeat 
        tempf=ReadString(0)
      Until Round(ValD(StringField(tempf,1,",")),#PB_Round_Nearest)=em_start Or tempf=""
      For i=0 To n_em
        For k=0 To n_samp_file 
          For j=0 To n_ex
            X(k+skip,i,j)=ValD(StringField(tempf,k*(n_ex_file+1)+j+2+ex_diff,","))  ;j+2 für erste Säule  ; 8 merken
     
        ;If (j=16 And i=0) Or (j=17 And (i=0 Or i=1)):X(k,i,j)=0:EndIf
          Next j
        Next k
      tempf=ReadString(0)
    Next i  
    skip+n_samp_file+1
    CloseFile(0)  
  Next l
   
  If absorbance
    
    tempfile.s=OpenFileRequester("Select Absorbance File",directory+"*.*","CSV (*.csv)|*.csv|Alle Dateien (*.*)|*.*",0)
    If ReadFile(0,tempfile)
      filefound=1
      n_abs.l=-1
      tempf=ReadString(0)
      tempf=ReadString(0)
      tempf=ReadString(0)
      tempf=ReplaceString(tempf,",",";") 
      abs_end.l=Round(ValD(StringField(tempf,1,";")),#PB_Round_Nearest)
      While Not ValD(StringField(tempf ,1,";"))=0
         n_abs+1
         tempf=ReplaceString(tempf,",",";")
         abs_start.l=Round(ValD(StringField(tempf,1,";")),#PB_Round_Nearest)
         tempf=ReadString(0)
       Wend      
       abs_step.l=(abs_end-abs_start)/n_abs
    Else
      filefound=0
      MessageRequester("Error","Could not open absorbance file") 
    EndIf   
    Global Dim absorb.d(n_samp,n_abs,0)
    CloseFile(0)
    If filefound
      ReadFile(0,tempfile)
      For k=0 To n_samp
        i=0
        FileSeek(0,0)
        tempf=ReadString(0)
        tempf=ReplaceString(tempf,",",";")
        Repeat
          i+1
          filename.s=StringField(tempf,i,";")
          seekname.s=RemoveString(GetFilePart(files(k)),".csv")
        Until FindString(filename,seekname) Or i=CountString(tempf,";")+1
        If FindString(filename,seekname)
          tempf=ReadString(0)
          For j=0 To n_abs
            tempf=ReadString(0)
            tempf=ReplaceString(tempf,",",";")  
            temp=ValD(StringField(tempf,i+1,";"))
            If temp>0
              absorb(k,n_abs-j,0)=temp
            Else
              absorb(k,n_abs-j,0)=0.000000000000001
            EndIf        
          Next j  
        Else
          If MessageRequester("Warning","UVVIS spectrum of "+seekname+" not found"+#eol+"Continue?",#PB_MessageRequester_YesNo)=#PB_MessageRequester_No:End:EndIf
        EndIf  
      Next k
      CloseFile(0)
    EndIf    
    
    
    
    For k=0 To n_samp
      For i=0 To n_em
        For j=0 To n_ex
          idx1.i=Round((ex_start-abs_start+j*ex_step)/abs_step,#PB_Round_Nearest)
          idx2.i=Round((em_start-abs_start+i*em_step)/abs_step,#PB_Round_Nearest)
                       
          X(k,i,j)=X(k,i,j)*Log(10)*absorb(k,idx,0)/(1-Pow(10,-absorb(k,idx1,0)))*Pow(10,0.13*absorb(k,idx2,0))*Log(10)*0.74*absorb(k,idx2,0)/(1-Pow(10,-0.74*absorb(k,idx2,0)))
          ;X(k,i,j)=X(k,i,j)*Pow(10,0.5*(absorb(k,(em_start-abs_start+i*em_step)/abs_step,0)+absorb(k,(ex_start-abs_start+j*ex_step)/abs_step,0)));-blank(0,i,j)         
        Next j 
      Next i  
    Next k
  EndIf  
    
  If correct
    For k=0 To n_samp
      For i=0 To n_em
        For j=0 To n_ex
          X(k,i,j)=X(k,i,j)*excorr(j*ex_step+exoffset)*emcorr(i*em_step+emoffset)
        Next j 
      Next i  
    Next k
  EndIf    
  
  CopyArray(X(),X_origin())
  
  Global Dim E.d(n_samp,n_em,n_ex)
  Global Dim X_recon.d(n_samp,n_em,n_ex)
  Global Dim HIX.d(n_samp)
  Global Dim comps.d(n_em,n_ex,n_comp)
  Global Dim A.d(n_samp,n_comp)
  Global Dim A_dev.d(n_samp,n_comp)
  Global Dim A_se.d(n_samp,n_comp)
  Global Dim A_sd.d(n_samp,n_comp)
  Global Dim B.d(n_em,n_comp)
  Global Dim B_dev.d(n_em,n_comp)
  Global Dim C_dev.d(n_ex,n_comp)
  Global Dim B_se.d(n_em,n_comp)
  Global Dim C_se.d(n_ex,n_comp)
  Global Dim B_sd.d(n_em,n_comp)
  Global Dim C_sd.d(n_ex,n_comp)
  Global Dim C.d(n_ex,n_comp)
  Global Dim core.d(n_comp,n_comp,n_comp)
  Global Dim fixed.i(n_comp)
  
  ex_hix=(254-ex_start)/ex_step
  em_hix1=(300-em_start)/em_step
  em_hix2=(345-em_start)/em_step
  em_hix3=(435-em_start)/em_step
  em_hix4=(480-em_start)/em_step
  If ex_hix>=0 And ex_hix<=n_ex And em_hix1>=0 And em_hix4<=n_em
    For i=0 To n_samp
      temp1.d=0
      temp2.d=0
      For j=em_hix1 To em_hix2
        temp1+X(i,j,ex_hix)
      Next j
      For j=em_hix3 To em_hix4
        temp2+X(i,j,ex_hix)
      Next j 
      hix(i)=temp2/temp1
    Next i 
  Else 
    For i=0 To n_samp
      hix(i)=NaN()
    Next i  
  EndIf
  
squaresum=0
mean=0
count=0
mean=0

max.d=-Pow(10,30)
min.d=Pow(10,30)
For i=0 To n_samp
  For j=0 To n_em
    For k=0 To n_ex
      If Not missing(j,k)
        If X(i,j,k)<min:min=X(i,j,k):EndIf
        If X(i,j,k)>max:max=X(i,j,k):EndIf
        mean+X(i,j,k)
        squaresum+Pow(X(i,j,k),2)
        count+1
      EndIf  
    Next k
  Next j
Next i
mean=mean/count
range=max-min

totvar=0
For i=0 To n_samp
  For j=0 To n_em
    For k=0 To n_ex
       If Not missing(j,k)
         totvar+Pow(X(i,j,k)-mean,2)
       EndIf  
    Next k
  Next j
Next i

For i=0 To n_samp
  files(i)=RemoveString(GetFilePart(files(i)),".csv")
Next i  
 
EndProcedure  

Procedure Import_state()
  
  tempf.s
  zeile.s
  tempf=PathRequester("Select Path", directory)
  If ReadFile(0,tempf+"component_concentration.txt")
    drawnumber=1
    zeile=ReadString(0)
    n_comp=CountString(zeile,"comp.")/4-1
    SetGadgetState(#Spin_n_comp,n_comp+1)
    For i=0 To n_comp_alt
      RemoveGadgetItem(#ListGadget,1)
    Next i
    nonneg_alt=nonneg
    n_comp_alt=n_comp
    ;ReDim best_A.d(n_samp,n_comp)
    ReDim A.d(n_samp,n_comp)
    ReDim B.d(n_em,n_comp)
    ReDim C.d(n_ex,n_comp)
    ReDim A_dev.d(n_samp,n_comp)
    ReDim B_dev.d(n_em,n_comp)
    ReDim C_dev.d(n_ex,n_comp)
    ReDim A_se.d(n_samp,n_comp)
    ReDim B_se.d(n_em,n_comp)
    ReDim C_se.d(n_ex,n_comp)
    ReDim A_sd.d(n_samp,n_comp)
    ReDim B_sd.d(n_em,n_comp)
    ReDim C_sd.d(n_ex,n_comp)
    ReDim comps.d(n_em,n_ex,n_comp)
    FreeArray(core())
    Global Dim core.d(n_comp,n_comp,n_comp)
    ;ReDim best_B.d(n_em,n_comp)
    ;ReDim best_C.d(n_ex,n_comp)
    ;ReDim last_A.d(n_samp,n_comp)
    ;ReDim last_B.d(n_em,n_comp)
    ;ReDim last_C.d(n_ex,n_comp)
    ;ReDim best_A_dev.d(n_samp,n_comp)
    ;ReDim best_B_dev.d(n_em,n_comp)
    ;ReDim best_C_dev.d(n_ex,n_comp)
    For i=0 To n_comp
      AddGadgetItem(#ListGadget,1,"component "+Str(n_comp+1-i))
    Next i
  
  If OpenFile(0,tempf+"component_concentration.txt")
    For i=0 To n_samp
      zeile=ReadString(0)
      For j=0 To n_comp
        A(i,j)=ValD(StringField(zeile,j+2,Chr(9)))
      Next j  
    Next i  
  EndIf
  CloseFile(0)
  If OpenFile(0,tempf+"component_em_spectra.txt")
    For i=0 To n_em
      zeile=ReadString(0)
      For j=0 To n_comp
        B(i,j)=ValD(StringField(zeile,j+2,Chr(9)))
      Next j  
    Next i  
  EndIf
  CloseFile(0)
  If OpenFile(0,tempf+"component_ex_spectra.txt")
    For i=0 To n_ex
      zeile=ReadString(0)
      For j=0 To n_comp
        C(i,j)=ValD(StringField(zeile,j+2,Chr(9)))
      Next j  
    Next i  
  EndIf 
  CloseFile(0)
  EndIf
EndProcedure  

Procedure import_fixed_spectra()
  
  tempf.s
  zeile.s
  max.d
  n_fixed_import.i=-1
  If Not generic
    tempf=PathRequester("Select Path", directory)
    If ReadFile(0,tempf+"component_em_spectra.txt") And ReadFile(1,tempf+"component_ex_spectra.txt")
      zeile=ReadString(0)
      FileSeek(0,0)
      n_fixed_import=CountString(zeile,Chr(9))-1
      If n_fixed_import>n_comp
        n_comp=n_fixed_import
      EndIf  
      SetGadgetState(#Spin_n_comp,n_comp+1)
      For i=0 To n_comp_alt
        RemoveGadgetItem(#ListGadget,2)
      Next i
      n_comp_alt=n_comp
      ReDim A.d(n_samp,n_comp)
      ReDim B.d(n_em,n_comp)
      ReDim C.d(n_ex,n_comp)
      ReDim A_dev.d(n_samp,n_comp)
      ReDim B_dev.d(n_em,n_comp)
      ReDim C_dev.d(n_ex,n_comp)
      ReDim A_se.d(n_samp,n_comp)
      ReDim B_se.d(n_em,n_comp)
      ReDim C_se.d(n_ex,n_comp)
      ReDim A_sd.d(n_samp,n_comp)
      ReDim B_sd.d(n_em,n_comp)
      ReDim C_sd.d(n_ex,n_comp)
      ReDim fixed.i(n_comp)
      ReDim comps.d(n_em,n_ex,n_comp)
      For i=0 To n_fixed_import
        fixed(i)=1
      Next i
      FreeArray(core())
      Global Dim core.d(n_comp,n_comp,n_comp)
      For i=0 To n_comp
        If fixed(n_comp-i)
          AddGadgetItem(#ListGadget,2,"component "+Str(n_comp+1-i)+" (fixed)")
        Else
          AddGadgetItem(#ListGadget,2,"component "+Str(n_comp+1-i))
        EndIf  
      Next i  
      For i=0 To n_em
        zeile=ReadString(0)
        For j=0 To n_fixed_import
          B(i,j)=ValD(StringField(zeile,j+2,Chr(9)))
          B_dev(i,j)=0
          B_se(i,j)=0
          B_sd(i,j)=0
        Next j  
      Next i
      For i=0 To n_ex
        zeile=ReadString(1)
        For j=0 To n_fixed_import
          C(i,j)=ValD(StringField(zeile,j+2,Chr(9)))
          C_dev(i,j)=0
          C_se(i,j)=0
          C_sd(i,j)=0
        Next j 
      Next i
      CloseFile(0)
      CloseFile(1)
    EndIf
  Else
    tempf=OpenFileRequester("Select Sample Files",directory+"*.*","CSV (*.csv)|*.csv|Alle Dateien (*.*)|*.*",0)
    If ReadFile(0,tempf)
      tempf=ReadString(0)
      separator=get_separator(tempf)
      n_fixed_import=CountString(tempf,separator)-1
      If n_fixed_import>n_comp
        n_comp=n_fixed_import
      EndIf  
      SetGadgetState(#Spin_n_comp,n_comp+1)
      For i=0 To n_comp_alt
        RemoveGadgetItem(#ListGadget,2)
      Next i
      n_comp_alt=n_comp
      ReDim A.d(n_samp,n_comp)
      ReDim B.d(n_em,n_comp)
      ReDim C.d(n_ex,n_comp)
      ReDim A_dev.d(n_samp,n_comp)
      ReDim B_dev.d(n_em,n_comp)
      ReDim C_dev.d(n_ex,n_comp)
      ReDim A_se.d(n_samp,n_comp)
      ReDim B_se.d(n_em,n_comp)
      ReDim C_se.d(n_ex,n_comp)
      ReDim A_sd.d(n_samp,n_comp)
      ReDim B_sd.d(n_em,n_comp)
      ReDim C_sd.d(n_ex,n_comp)
      ReDim fixed.i(n_comp)
      ReDim comps.d(n_em,n_ex,n_comp)
      For i=0 To n_fixed_import
        fixed(i)=1
      Next i
      FreeArray(core())
      Global Dim core.d(n_comp,n_comp,n_comp)
      For i=0 To n_comp
        If fixed(n_comp-i)
          AddGadgetItem(#ListGadget,2,"component "+Str(n_comp+1-i)+" ("+StringField(tempf,n_comp-i+2,separator)+")")
        Else
          AddGadgetItem(#ListGadget,2,"component "+Str(n_comp+1-i))
        EndIf  
      Next i
      generic_names=tempf
      Repeat
        i+1
        tempf=ReadString(0)
      Until ValD(StringField(tempf,1,separator))>ex_start Or Eof(0)
      
      For i=0 To n_ex
        For j=0 To n_fixed_import
          C(i,j)=ValD(StringField(tempf,j+2,separator))
          C_dev(i,j)=0
          C_se(i,j)=0
          C_sd(i,j)=0         
        Next j
        tempf=ReadString(0)
      Next i
      CloseFile(0)
      For i=0 To n_fixed_import
        max=-Pow(10,30)
        For j=0 To n_ex
          If C(j,i)>max:max=C(j,i):EndIf
        Next
        For j=0 To n_ex
          C(j,i)/max
        Next
      Next i  
    EndIf
  EndIf
  CallDebugger
  n_fixed=n_fixed_import
  n_fixed_alt=n_fixed_import
  
EndProcedure  

Procedure Punkt(x.i,y.i,dickex.d,dickey.d,farbe.l)
  
  For i=0 To Round(dickex-1,#PB_Round_Up)
    For j=0 To Round(dickey-1,#PB_Round_Up)
      Plot(x+i,y-j,farbe)  
    Next j    
  Next i  
  
EndProcedure  
  
Procedure.l gradient(nrd.l)
  ;skaliert auf 0-1020, d.h. argument nr von 0 bis 1020 gibt vollen farbkreis
    result.l
  ;nr=nr*abstufungen/6
  If grayscale
    nr=Round(nrd/4,#PB_Round_Nearest)
    nr-255*Int((nr-254)/255)
    While Not nr<=255
      nr-255;-255
    Wend
    If nr<0:nr=0:EndIf
    result=RGB(255-nr,255-nr,255-nr)
    ProcedureReturn result
  Else  
nr=Round(nrd,#PB_Round_Nearest)
  While Not nr<1276
    nr-1531;-255
  Wend
  While Not nr>-256
    nr+1531
  Wend  
  ;While Not nr>=0
  ;  nr+1021
  ;Wend
  If nr<0
    nr+255
    result=RGBA(255-nr,0,255,255)    
  ElseIf nr<256
    result=RGBA(0,nr,255,255)
  ElseIf nr<511
    nr-255
    result=RGBA(0,255,255-nr,255)
  ElseIf nr<766
    nr-510
    result=RGBA(nr,255,0,255)
  ElseIf nr<1021
    nr-765
    result=RGBA(255,255-nr,0,255)
  ElseIf nr<1276
    nr-1020
    result=RGBA(255,0,nr,255)
  EndIf  
  ProcedureReturn result  
  EndIf
EndProcedure  

Procedure.l contour(nr.l);,abstufungen.i)
  result.l
  ;If (nr+1)%100=0 Or (nr+2)%100=0 Or (nr+3)%100=0 Or (nr+4)%100=0 Or (nr+5)%100=0 Or (nr+6)%100=0 Or (nr+7)%100=0 Or (nr+8)%100=0 Or (nr+9)%100=0 Or (nr+10)%100=0
  ;abstufungen=Round(1020/abstufungen,#PB_Round_Down)
  ;result=nr%abstufungen
  ;result=Abs(result-abstufungen/2)*1020/abstufungen
  ;If result>255:result=255:EndIf
  ;result=RGB(result,result,result)
  ;Else
  ;  result=RGB(255,255,255)
  ;EndIf
  If nr:result=RGB(0,0,0):Else:result=RGB(255,255,255):EndIf
  ProcedureReturn result    
EndProcedure  

Procedure.l farbe(nr.l)
 
  result.l
  While nr>24
    nr-24
  Wend  
  Select nr
    Case 0:result=RGBA(0,0,0,255)  
    Case 1:result=RGBA(170,100,100,255)      
    Case 2:result=RGBA(100,170,100,255) 
    Case 3:result=RGBA(100,100,170,255)
    Case 4:result=RGBA(130,130,130,255) 
    Case 5:result=RGBA(170,100,170,255) 
    Case 6:result=RGBA(170,170,100,255) 
    Case 7:result=RGBA(100,170,170,255) 
    Case 8:result=RGBA(150,150,150,255) 
    Case 9:result=RGBA(70,70,70,255) 
    Case 10:result=RGBA(230,70,70,255)
    Case 11:result=RGBA(70,230,70,255) 
    Case 12:result=RGBA(70,70,230,255) 
    Case 13:result=RGBA(230,230,30,255) 
    Case 14:result=RGBA(230,30,230,255) 
    Case 15:result=RGBA(30,230,230,255) 
    Case 16:result=RGBA(255,0,0,255) 
    Case 17:result=RGBA(0,255,0,255) 
    Case 18:result=RGBA(0,0,255,255)
    Case 19:result=RGBA(100,0,255,255) 
    Case 20:result=RGBA(0,100,255,255) 
    Case 21:result=RGBA(0,255,100,255) 
    Case 22:result=RGBA(100,255,0,255) 
    Case 23:result=RGBA(255,0,100,255)    
    Case 24:result=RGBA(255,100,0,255) 
  EndSelect
  
  ProcedureReturn result  
  
EndProcedure 

Procedure.l schwache_farbe(nr.l)
  farb.l=farbe(nr)
  ProcedureReturn RGBA(Red(farb),Green(farb),Blue(farb),100)
  ;ProcedureReturn RGBA(255-0.9*(255-Red(farb)),255-0.9*(255-Green(farb)),255-0.9*(255-Blue(farb)),50)
EndProcedure  

Procedure.l lichte_farbe(nr.l)
  farb.l=farbe(nr)
  ;ProcedureReturn RGBA(Red(farb),Green(farb),Blue(farb),100)
  ProcedureReturn RGB(255-0.7*(255-Red(farb)),255-0.7*(255-Green(farb)),255-0.7*(255-Blue(farb)))
EndProcedure 

Procedure set_scale()
  max.d=-Pow(10,30)
  min.d=Pow(10,30)
  
  
  ;For i=1 To iteration
  ;  If ssqs(i)>max:max=ssqs(i):EndIf
  ;  If ssqs(i)<min:min=ssqs(i):EndIf
  ;Next i 
  ;If max<=min:max=1:min=0:EndIf
  ;max=Round(max,#PB_Round_Up)
  ;min=Round(min,#PB_Round_Down)
  ;If iteration>1
  ;  scalex=(1200)/(iteration-1)
  ;Else
  ;  scalex=1200
  ;EndIf 
  ;scaley=400/(max-min)
  ;middle=(max+min)/2
  
  max_spec=1;Round(max_spec,#PB_Round_Up)
  min_spec=0;Round(min_spec,#PB_Round_Down)
  scalex_em=560/n_em
  scalex_ex=560/n_ex 
  scaley_spectra=400/(max_spec-min_spec)
  
EndProcedure

Procedure contourline(Array sourcearray.i(2),x.i,y.i)
  
  dasistdiezahl.i=sourcearray(x,y)
  If sourcearray(x-1,y-1)<>dasistdiezahl:ProcedureReturn 1:EndIf
  If sourcearray(x-1,y)<>dasistdiezahl:ProcedureReturn 1:EndIf
  If sourcearray(x,y-1)<>dasistdiezahl:ProcedureReturn 1:EndIf
  
  ProcedureReturn 0
  
EndProcedure  

Procedure convert_to_contour(Array sourcearray.i(2),x.i,y.i)
  
  Dim temp.i(x,y)
  For i=1 To x
    For j=1 To y
      temp(i,j)=contourline(sourcearray(),i,j)
    Next j  
  Next i  
  CopyArray(temp(),sourcearray())
  
EndProcedure  

Procedure scalebars_and_legend(max.d,min.d,normal.b)
  
  scaletext.s
  scalerange_z.d=max-min
    startscale_z.d=Round(min/interval_z,#PB_Round_Up)*interval_z
    endscale_z.d= Round(max/interval_z,#PB_Round_Down)*interval_z   
    n_ticks_z.i=Round((endscale_z-startscale_z)/interval_z,#PB_Round_Nearest)
    
    If normal And Not GetGadgetState(#Checkbox_contour)=#PB_Checkbox_Checked
  DrawingMode(#PB_2DDrawing_Transparent)
        last_y.i=365
        DrawText(2*n_em*scaleplot_x+200,360,"raman units",RGB(0,0,0),RGB(255,255,255))
        For i=0 To n_ticks_z
          ypoint.i=350-Round(i/(n_ticks_z+1)*(n_ex+1)*scaleplot_y,#PB_Round_Nearest)
          height.i=Round((n_ex+1)*scaleplot_y/(n_ticks_z+1),#PB_Round_Up)
          Box(2*n_em*scaleplot_x+200,ypoint,50,-height,gradient(Round((startscale_z+i*(endscale_z-startscale_z)/(n_ticks_z))/max*1020,#PB_Round_Nearest)))
          If height>15
            ypoint-(height-15)/2
          EndIf         
          If last_y-ypoint>=15
            last_y=ypoint
            scaletext=StrD(startscale_z+i*interval_z,decimals_z)
            DrawText(2*n_em*scaleplot_x+260,ypoint-15,scaletext)
          EndIf  
        Next i 
        DrawingMode(#PB_2DDrawing_Default)
      EndIf  
        
      For i=0 To n_ticks_em
      xpoint.i=100+((startscale_em+i*interval)-scale_em(0))/(scalerange_em)*n_em*scaleplot_x
      LineXY(xpoint,350,xpoint,352,RGB(0,0,0))
      If normal:LineXY(xpoint+50+n_em*scaleplot_x,350,xpoint+50+n_em*scaleplot_x,352,RGB(0,0,0)):EndIf
      scaletext=StrD(startscale_em+i*interval,decimals)    
      DrawText(xpoint-Len(scaletext)*4,360,scaletext)
      If normal:DrawText(xpoint+n_em*scaleplot_x+50-Len(scaletext)*4,360,scaletext):EndIf
    Next i 

    For i=0 To n_ticks_ex
      ypoint.i=350-((startscale_ex+i*interval)-scale_ex(0))/(scalerange_ex)*n_ex*scaleplot_y
      LineXY(98,ypoint,100,ypoint,RGB(0,0,0))
      If normal:LineXY(98+n_em*scaleplot_x+50,ypoint,100+n_em*scaleplot_x+50,ypoint,RGB(0,0,0)):EndIf
      scaletext=StrD(startscale_ex+i*interval,decimals)    
      DrawText(100-Len(scaletext)*10,ypoint-8,scaletext)
    Next i
    
    mitte.i=(n_em*scaleplot_x-30)/2
    If mitte<0:mitte=0:EndIf
    DrawingFont(FontID(symbolfont))
    DrawText(100+mitte,380,Chr(108),RGB(0,0,0),RGB(255,255,255))
    DrawingFont(#PB_Default)
    DrawText(112+mitte,390,"em",RGB(0,0,0),RGB(255,255,255))
    DrawText(140+mitte,388,"[nm]",RGB(0,0,0),RGB(255,255,255))
    If normal
    DrawingFont(FontID(symbolfont))
    DrawText(150+mitte+n_em*scaleplot_x,380,Chr(108),RGB(0,0,0),RGB(255,255,255))
    DrawingFont(#PB_Default)
    DrawText(162+mitte+n_em*scaleplot_x,390,"em",RGB(0,0,0),RGB(255,255,255))
    DrawText(190+mitte+n_em*scaleplot_x,388,"[nm]",RGB(0,0,0),RGB(255,255,255))
    EndIf
    mitte.i=(n_ex*scaleplot_y-30)/2
    If mitte<0:mitte=0:EndIf
    DrawingFont(FontID(symbolfont))
    DrawRotatedText(60-Len(scaletext)*10,350-mitte+30,Chr(108),90,RGB(0,0,0))
    DrawingFont(#PB_Default)
    DrawRotatedText(70-Len(scaletext)*10,350-mitte-12+30,"ex",90,RGB(0,0,0))
    DrawRotatedText(68-Len(scaletext)*10,350-mitte-40+30,"[nm]",90,RGB(0,0,0))
EndProcedure  

Procedure draw_plot()
  
  max.d
  min.d
  ueks.i
  If GetGadgetState(#CheckBox_component_position)=#PB_Checkbox_Checked
    Dim coord.xy(n_comp)
    For i=0 To n_comp
      max=0
      For j=1 To n_em
        If B(j,i)>max:max=B(j,i):ueks=j:EndIf
      Next j
      coord(i)\x=ueks
      max=0
      For j=1 To n_ex
        If C(j,i)>max:max=C(j,i):ueks=j:EndIf
      Next j
      coord(i)\y=ueks
    Next i  
  EndIf
  
  balkenbreite.i=0
  set_scale() 
  bildchen=CreateImage(#PB_Any,breite,550)

  StartDrawing(ImageOutput(bildchen))
  DrawingMode(#PB_2DDrawing_Default)
  BackColor(RGB(255,255,255))
  FrontColor(RGB(0,0,0))
  Box(0,0,breite,550,RGB(255,255,255))
  
  calculationtime=ElapsedMilliseconds()-calculationtime
  scaletext.s
  
  If drawnumber<0
    If Not twoD_comps Or drawnumber=-1-n_comp-2
      For i=0 To n_ticks_em
        xpoint.i=680+((startscale_em+i*interval)-scale_em(0))/(scalerange_em)*560
        LineXY(xpoint,440,xpoint,442,RGB(0,0,0))
        scaletext=StrD(startscale_em+i*interval,decimals)    
        DrawText(xpoint-Len(scaletext)*4,450,scaletext)      
      Next i
      
      If Not drawnumber=-1-n_comp-1
        DrawingMode(#PB_2DDrawing_Transparent)
        DrawingFont(FontID(symbolfont))
        DrawText(900,460,Chr(108))
        DrawText(260,460,Chr(108))
        DrawingFont(#PB_Default)
        DrawText(912,470,"em")
        DrawText(272,470,"ex")
        DrawText(940,468,"[nm]")
        DrawText(300,468,"[nm]")  
        DrawingMode(#PB_2DDrawing_Default)
      EndIf
      
      For i=0 To n_ticks_ex
        xpoint.i=40+((startscale_ex+i*interval)-scale_ex(0))/(scalerange_ex)*560
        LineXY(xpoint,440,xpoint,442,RGB(0,0,0))
        scaletext=StrD(startscale_ex+i*interval,decimals)    
        DrawText(xpoint-Len(scaletext)*4,450,scaletext)
      Next i
      For i=1 To 280
        Plot(2*i+40,440)
      Next i
      For i=320 To 600
        Plot(2*i+40,440)
      Next i
    EndIf
    DrawingMode(#PB_2DDrawing_AlphaBlend)
    Dim drawB.d(n_em,n_comp)
    Dim drawC.d(n_ex,n_comp)
    Dim drawdevB1.d(n_em,n_comp)
    ;Dim drawdevB2.d(n_em,n_comp)
    Dim drawdevC1.d(n_ex,n_comp)
    ;Dim drawdevC2.d(n_ex,n_comp)
    
    For i=0 To n_comp
      For j=0 To n_em
        drawB(j,i)=Round((B(j,i)-0.5)*scaley_spectra,#PB_Round_Nearest)
      Next j
      For j=0 To n_ex
        drawC(j,i)=Round((C(j,i)-0.5)*scaley_spectra,#PB_Round_Nearest)
      Next j  
    Next i  
    
    Select kindofdev
      Case 0
        For i=0 To n_comp
          For j=0 To n_em
            drawdevB1(j,i)=Round(scaledev*B_dev(j,i)*scaley_spectra,#PB_Round_Nearest)
          Next j
          For j=0 To n_ex
            drawdevC1(j,i)=Round(scaledev*C_dev(j,i)*scaley_spectra,#PB_Round_Nearest)
          Next j  
        Next i  
      Case 1
        For i=0 To n_comp
          For j=0 To n_em
            drawdevB1(j,i)=Round(scaledev*B_se(j,i)*scaley_spectra,#PB_Round_Nearest)
          Next j
          For j=0 To n_ex
            drawdevC1(j,i)=Round(scaledev*C_se(j,i)*scaley_spectra,#PB_Round_Nearest)
          Next j  
        Next i 
      Case 2
        For i=0 To n_comp
          For j=0 To n_em
            drawdevB1(j,i)=Round(scaledev*B_sd(j,i)*scaley_spectra,#PB_Round_Nearest)
          Next j
          For j=0 To n_ex
            drawdevC1(j,i)=Round(scaledev*C_sd(j,i)*scaley_spectra,#PB_Round_Nearest)
          Next j  
        Next i 
    EndSelect    
    
    If drawnumber=-1-n_comp-2                           ;alle Spektren zusammen
      For i=0 To n_comp
        ueks=0-Round(scalex_em/2,#PB_Round_Nearest)
        For j=1 To n_em
          balkenbreite=Round(j*scalex_em,#PB_Round_Nearest)-Round((j-1)*scalex_em,#PB_Round_Nearest)
          Box(ueks+680,240-drawB(j-1,i)-drawdevB1(j-1,i),balkenbreite,2*drawdevB1(j-1,i),schwache_farbe(i))
          LineXY(Round((j-1)*scalex_em,#PB_Round_Nearest)+680,240-drawB(j-1,i),Round(j*scalex_em,#PB_Round_Nearest)+680,240-drawB(j,i),farbe(i))
          ueks+balkenbreite  
        Next j
        Box(ueks+680,240-drawB(j-1,i)-drawdevB1(j-1,i),balkenbreite,2*drawdevB1(j-1,i),schwache_farbe(i))
        ueks=0-Round(scalex_ex/2,#PB_Round_Nearest)
        For j=1 To n_ex
          balkenbreite=Round(j*scalex_ex,#PB_Round_Nearest)-Round((j-1)*scalex_ex,#PB_Round_Nearest)
          Box(ueks+40,240-drawC(j-1,i)-drawdevC1(j-1,i),balkenbreite,2*drawdevC1(j-1,i),schwache_farbe(i))
          LineXY(Round((j-1)*scalex_ex,#PB_Round_Nearest)+40,240-drawC(j-1,i),Round(j*scalex_ex,#PB_Round_Nearest)+40,240-drawC(j,i),farbe(i))  
          ueks+balkenbreite  
        Next j
        Box(ueks+40,240-drawC(j-1,i)-drawdevC1(j-1,i),balkenbreite,2*drawdevC1(j-1,i),schwache_farbe(i))
      Next i 
    ElseIf drawnumber=-1-n_comp-1                       ;Verteilung der Gehalte
      DrawingMode(#PB_2DDrawing_Default)
      Box(1,430,1330,40,RGBA(255,255,255,255))
      chartbreite.i=Round(1200/((n_comp+1)*(n_samp+1)+n_samp),#PB_Round_Down)
      max=0
      For i=0 To n_samp
        For j=0 To n_comp
          If A(i,j)>max:max=A(i,j):EndIf
        Next j  
      Next i
      For i=0 To n_samp
        LineXY(40+chartbreite*(i*(n_comp+2)),440,40+chartbreite*(i*(n_comp+2)+n_comp+1),440,RGBA(100,100,100,255))
        LineXY(40+chartbreite*(i*(n_comp+2)),441,40+chartbreite*(i*(n_comp+2)+n_comp+1),441,RGBA(100,100,100,255))
        DrawText(40+chartbreite*(i*(n_comp+2)),443,files(i),RGBA(0,0,0,255))
        For j=0 To n_comp
          Box(40+chartbreite*(i*(n_comp+2)+j),440,chartbreite,-A(i,j)/max*400,farbe(j))
          Select kindofdev
            Case 0  
              LineXY(40+chartbreite*(i*(n_comp+2)+j)+chartbreite/2,440-(A(i,j)+A_dev(i,j)*scaledev)/max*400,40+chartbreite*(i*(n_comp+2)+j)+chartbreite/2,440-(A(i,j)-A_dev(i,j)*scaledev)/max*400,lichte_farbe(j))
            Case 1
              LineXY(40+chartbreite*(i*(n_comp+2)+j)+chartbreite/2,440-(A(i,j)+A_se(i,j)*scaledev)/max*400,40+chartbreite*(i*(n_comp+2)+j)+chartbreite/2,440-(A(i,j)-A_se(i,j)*scaledev)/max*400,lichte_farbe(j))
            Case 2
              LineXY(40+chartbreite*(i*(n_comp+2)+j)+chartbreite/2,440-(A(i,j)+A_sd(i,j)*scaledev)/max*400,40+chartbreite*(i*(n_comp+2)+j)+chartbreite/2,440-(A(i,j)-A_sd(i,j)*scaledev)/max*400,lichte_farbe(j))
          EndSelect
        Next j  
      Next i
    Else                                    ;alles andere - einzelnen Komponenten und Proben
      If twoD_comps                                        ;Komponenten als 2D
        If Not GetGadgetState(#Checkbox_contour)=#PB_Checkbox_Checked
          DrawingMode(#PB_2DDrawing_Default)
          For i=0 To n_em
            For j=0 To n_ex
              Punkt(i*scaleplot_x+100,350-j*scaleplot_y,scaleplot_x,scaleplot_y,gradient(Round(comps(i,j,drawnumber+n_comp+1)*1020,#PB_Round_Nearest)))
            Next j  
          Next i
        Else
          Dim component.i(n_em,n_ex)
          For i=0 To n_em
            For j=0 To n_ex
              component(i,j)=Round(comps(i,j,drawnumber+n_comp+1)/interval_z,#PB_Round_Down)
            Next j
          Next i
          
          tmp.i=Round(1/interval_z,#PB_Round_Down)+20
          Dim pos_component.xy(tmp)
          Dim len_component.i(tmp)
          count_component.i
          count_component_inv.i
          For j=1 To n_ex
            count_component=0
            count_component_inv=0
            For i=n_em To 0 Step -1
              If component(i,j)+1=component(i,j-1)
                count_component+1
                If count_component>len_component(component(i,j-1)+20)
                  pos_component(component(i,j-1)+20)\x=i
                  pos_component(component(i,j-1)+20)\y=j
                  len_component(component(i,j-1)+20)=count_component
                EndIf
              Else 
                count_component=0
              EndIf
              If component(i,j-1)+1=component(i,j)
                count_component_inv+1
                If count_component_inv>len_component(component(i,j)+20)
                  pos_component(component(i,j)+20)\x=i
                  pos_component(component(i,j)+20)\y=j
                  len_component(component(i,j)+20)=count_component_inv
                EndIf
              Else
                count_component_inv=0
              EndIf  
            Next i  
          Next j  
          
          convert_to_contour(component(),n_em,n_ex)
          DrawingMode(#PB_2DDrawing_Default)
          For i=0 To n_em
            For j=0 To n_ex
              Punkt(i*scaleplot_x+100,350-j*scaleplot_y,scaleplot_x,scaleplot_y,contour(component(i,j)))
            Next j  
          Next i
          For i=0 To tmp
            If pos_component(i)\x>0 And pos_component(i)\y<n_ex
              DrawText(95+len_component(i)/2+scaleplot_x*pos_component(i)\x,342-scaleplot_y*pos_component(i)\y,StrD((i-20)*interval_z,decimals_z),RGB(255,255,255))
            EndIf
          Next i
          DrawingMode(#PB_2DDrawing_Transparent)
          For i=0 To tmp
            If pos_component(i)\x>0 And pos_component(i)\y<n_ex
              DrawText(95+len_component(i)/2+scaleplot_x*pos_component(i)\x,342-scaleplot_y*pos_component(i)\y,StrD((i-20)*interval_z,decimals_z),RGB(0,0,0))
            EndIf 
          Next i
          DrawingMode(#PB_2DDrawing_Default)
          
        EndIf  
        scalebars_and_legend(1,0,0)
      Else                                                  ;Komponenten als Spektren
        ueks=0-Round(scalex_em/2,#PB_Round_Nearest)
        For j=1 To n_em
          balkenbreite=Round(j*scalex_em,#PB_Round_Nearest)-Round((j-1)*scalex_em,#PB_Round_Nearest)
          Box(ueks+680,240-drawB(j-1,drawnumber+n_comp+1)-drawdevB1(j-1,drawnumber+n_comp+1),balkenbreite,2*drawdevB1(j-1,drawnumber+n_comp+1),schwache_farbe(drawnumber+n_comp+1))
          LineXY(Round((j-1)*scalex_em,#PB_Round_Nearest)+680,240-drawB(j-1,drawnumber+n_comp+1),Round(j*scalex_em,#PB_Round_Nearest)+680,240-drawB(j,drawnumber+n_comp+1),farbe(drawnumber+n_comp+1))
          ueks+balkenbreite  
        Next j
        Box(Round((j-1)*scalex_em,#PB_Round_Nearest)-Round(balkenbreite/2,#PB_Round_Down)+680,240-drawB(j-1,drawnumber+n_comp+1)-drawdevB1(j-1,drawnumber+n_comp+1),balkenbreite,2*drawdevB1(j-1,drawnumber+n_comp+1),schwache_farbe(drawnumber+n_comp+1))
        ueks=0-Round(scalex_ex/2,#PB_Round_Nearest)
        For j=1 To n_ex
          balkenbreite=Round(j*scalex_ex,#PB_Round_Nearest)-Round((j-1)*scalex_ex,#PB_Round_Nearest)
          Box(ueks+40,240-drawC(j-1,drawnumber+n_comp+1)-drawdevC1(j-1,drawnumber+n_comp+1),balkenbreite,2*drawdevC1(j-1,drawnumber+n_comp+1),schwache_farbe(drawnumber+n_comp+1))
          LineXY(Round((j-1)*scalex_ex,#PB_Round_Nearest)+40,240-drawC(j-1,drawnumber+n_comp+1),Round(j*scalex_ex,#PB_Round_Nearest)+40,240-drawC(j,drawnumber+n_comp+1),farbe(drawnumber+n_comp+1))  
          ueks+balkenbreite  
        Next j
        Box(Round((j-1)*scalex_ex,#PB_Round_Nearest)-Round(balkenbreite/2,#PB_Round_Down)+40,240-drawC(j-1,drawnumber+n_comp+1)-drawdevC1(j-1,drawnumber+n_comp+1),balkenbreite,2*drawdevC1(j-1,drawnumber+n_comp+1),schwache_farbe(drawnumber+n_comp+1))
        For i=1 To 280
          Plot(2*i+40,440)
        Next i
        For i=320 To 600
          Plot(2*i+40,440)
        Next i  
      EndIf
      
      
    EndIf
    DrawingMode(#PB_2DDrawing_Default)
    DrawText(40,510,"current RMSE/NRMSE/CV(RMSE): "+StrD(ssq)+" "+StrD(nssq)+" "+StrD(cvssq))
    DrawText(640,510,"relative change: "+StrD(Log10(1-ratio),3)+" in "+Str(calculationtime)+"ms")
  Else    ;alle Proben
    Dim original.i(n_em,n_ex)
    Dim recon.i(n_em,n_ex)
    Dim resid.i(n_em,n_ex)
    
    ;Get Scale for 2D-Plots
    
    If GetGadgetState(#CheckBox_relative_contents)
      
      max=-Pow(10,30)
      min=Pow(10,30)
      For i=0 To n_em
        For j=0 To n_ex
          If X(drawnumber,i,j)>max And Not missing(i,j):max=X(drawnumber,i,j):EndIf
          If X_recon(drawnumber,i,j)>max:max=X_recon(drawnumber,i,j):EndIf
          If E(drawnumber,i,j)>max:max=E(drawnumber,i,j):EndIf
          If E(drawnumber,i,j)<min:min=E(drawnumber,i,j):EndIf
        Next j  
      Next i
      
    Else
      
      max=-Pow(10,30)
      min=Pow(10,30)
      For k=0 To n_samp
        For i=0 To n_em
          For j=0 To n_ex
            If X(k,i,j)>max And Not missing(i,j):max=X(k,i,j):EndIf
            If X_recon(k,i,j)>max:max=X_recon(k,i,j):EndIf
            If E(k,i,j)>max:max=E(k,i,j):EndIf
            If E(k,i,j)<min:min=E(k,i,j):EndIf
          Next j  
        Next i
      Next k
      
    EndIf

    ;Scale Data
    
    For i=0 To n_em
      For j=0 To n_ex
        original(i,j)=Round(X_origin(drawnumber,i,j)/max*1020,#PB_Round_Nearest)
        recon(i,j)=Round(X_recon(drawnumber,i,j)/max*1020,#PB_Round_Nearest)
        resid(i,j)=Round(scaledev*E(drawnumber,i,j)/max*1020,#PB_Round_Nearest);+510
      Next j  
    Next i
    
    ;2D-Plots
    
    If Not GetGadgetState(#Checkbox_rmse)
      DrawText(100,10,"original")
    Else
      If Not (calccore)
        DrawText(100,10,"reconstructed")
      Else
        DrawText(100,10,"reconstructed (Tucker)")
      EndIf  
    EndIf
    DrawText(150+n_em*scaleplot_x,10,"residual")
    If Not GetGadgetState(#Checkbox_contour)=#PB_Checkbox_Checked
      If GetGadgetState(#Checkbox_rmse)
        For i=0 To n_em
          For j=0 To n_ex
           ; If Not missing(i,j)
              Punkt(i*scaleplot_x+100,350-j*scaleplot_y,scaleplot_x,scaleplot_y,gradient(recon(i,j)))
              Punkt((i+n_em)*scaleplot_x+150,350-j*scaleplot_y,scaleplot_x,scaleplot_y,gradient(resid(i,j)))
           ; EndIf
          Next j  
        Next i
      Else
        For i=0 To n_em
          For j=0 To n_ex
           ; If Not missing(i,j)
              Punkt(i*scaleplot_x+100,350-j*scaleplot_y,scaleplot_x,scaleplot_y,gradient(original(i,j)))
              Punkt((i+n_em)*scaleplot_x+150,350-j*scaleplot_y,scaleplot_x,scaleplot_y,gradient(resid(i,j)))
           ; EndIf
          Next j  
        Next i
      EndIf
    Else                            ; CONTOUR FTW
      For i=0 To n_em
        For j=0 To n_ex
          recon(i,j)=Round(X_recon(drawnumber,i,j)/interval_z,#PB_Round_Down):If recon(i,j)<-200:recon(i,j)=-200:EndIf
          original(i,j)=Round(X(drawnumber,i,j)/interval_z,#PB_Round_Down):If original(i,j)<-200:original(i,j)=-200:EndIf
          resid(i,j)=Round(scaledev*E(drawnumber,i,j)/interval_z,#PB_Round_Down):If resid(i,j)<-200:resid(i,j)=-200:EndIf
        Next j
      Next i
      
      tmp.i=Round(max/interval_z,#PB_Round_Down)+200
      Dim pos_orig.xy(tmp)
      Dim pos_recon.xy(tmp)
      Dim pos_resid.xy(tmp)
      Dim len_orig.i(tmp)
      Dim len_recon.i(tmp)
      Dim len_resid.i(tmp)
      count_orig.i
      count_recon.i
      count_resid.i
      count_orig_inv.i
      count_recon_inv.i
      count_resid_inv.i
      For j=1 To n_ex
        count_orig=0
        count_orig_inv=0
        count_recon=0
        count_recon_inv=0
        count_resid=0
        count_resid_inv=0
        For i=n_em To 0 Step -1
          If original(i,j)+1=original(i,j-1)
            count_orig+1
            If count_orig>len_orig(original(i,j-1)+200)
              pos_orig(original(i,j-1)+200)\x=i
              pos_orig(original(i,j-1)+200)\y=j
              len_orig(original(i,j-1)+200)=count_orig
            EndIf
          Else 
            count_orig=0
          EndIf
          If original(i,j-1)+1=original(i,j)
            count_orig_inv+1
            If count_orig_inv>len_orig(original(i,j)+200)
              pos_orig(original(i,j)+200)\x=i
              pos_orig(original(i,j)+200)\y=j
              len_orig(original(i,j)+200)=count_orig_inv
            EndIf
          Else
            count_orig_inv=0
          EndIf  
          If recon(i,j)+1=recon(i,j-1)
            count_recon+1
            If count_recon>len_recon(recon(i,j-1)+200)
              pos_recon(recon(i,j-1)+200)\x=i 
              pos_recon(recon(i,j-1)+200)\y=j
              len_recon(recon(i,j-1)+200)=count_recon
            EndIf
          Else
            count_recon=0
          EndIf
          If recon(i,j-1)+1=recon(i,j)
            count_recon_inv+1
            If count_recon_inv>len_recon(recon(i,j)+200)
              pos_recon(recon(i,j)+200)\x=i
              pos_recon(recon(i,j)+200)\y=j
              len_recon(recon(i,j)+200)=count_recon_inv
            EndIf
          Else
            count_recon_inv=0
          EndIf 
          If resid(i,j)+1=resid(i,j-1)
            count_resid+1
          Else
            If count_resid>len_resid(resid(i,j-1)+200)
              len_resid(resid(i,j-1)+200)=count_resid
              pos_resid(resid(i,j-1)+200)\x=i
              pos_resid(resid(i,j-1)+200)\y=j
            EndIf
            count_resid=0
          EndIf
          If resid(i,j-1)+1=resid(i,j)
            count_resid_inv+1
          Else
            If count_resid_inv>len_resid(resid(i,j)+200)
              len_resid(resid(i,j)+200)=count_resid_inv
              pos_resid(resid(i,j)+200)\x=i
              pos_resid(resid(i,j)+200)\y=j
            EndIf
            count_resid_inv=0
          EndIf
        Next i  
      Next j  
      
      convert_to_contour(original(),n_em,n_ex)
      convert_to_contour(resid(),n_em,n_ex)      
      convert_to_contour(recon(),n_em,n_ex)
      
      If GetGadgetState(#Checkbox_rmse)
        For i=0 To n_em
          For j=0 To n_ex
            Punkt(i*scaleplot_x+100,350-j*scaleplot_y,scaleplot_x,scaleplot_y,contour(recon(i,j)))
            Punkt((i+n_em)*scaleplot_x+150,350-j*scaleplot_y,scaleplot_x,scaleplot_y,contour(resid(i,j)))
          Next j  
        Next i
        For i=0 To tmp
          If pos_recon(i)\x>0 And pos_recon(i)\y<n_ex
            DrawText(95+len_recon(i)/2+scaleplot_x*pos_recon(i)\x,342-scaleplot_y*pos_recon(i)\y,StrD((i-200)*interval_z,decimals_z),RGB(255,255,255))
          EndIf
          If pos_resid(i)\x>0 And pos_resid(i)\y<n_ex
            DrawText(145+len_resid(i)/2+scaleplot_x*(n_em+pos_resid(i)\x),342-scaleplot_y*pos_resid(i)\y,StrD((i-200)*interval_z,decimals_z),RGB(255,255,255))
          EndIf 
        Next i
        DrawingMode(#PB_2DDrawing_Transparent)
        For i=0 To tmp
          If pos_recon(i)\x>0 And pos_recon(i)\y<n_ex
            DrawText(95+len_recon(i)/2+scaleplot_x*pos_recon(i)\x,342-scaleplot_y*pos_recon(i)\y,StrD((i-200)*interval_z,decimals_z),RGB(0,0,0))
          EndIf
          If pos_resid(i)\x>0 And pos_resid(i)\y<n_ex
            DrawText(145+len_resid(i)/2+scaleplot_x*(n_em+pos_resid(i)\x),342-scaleplot_y*pos_resid(i)\y,StrD((i-200)*interval_z,decimals_z),RGB(0,0,0))
          EndIf 
        Next i
        DrawingMode(#PB_2DDrawing_Default)
      Else
        For i=0 To n_em
          For j=0 To n_ex
            Punkt(i*scaleplot_x+100,350-j*scaleplot_y,scaleplot_x,scaleplot_y,contour(original(i,j)))
            Punkt((i+n_em)*scaleplot_x+150,350-j*scaleplot_y,scaleplot_x,scaleplot_y,contour(resid(i,j)))
          Next j  
        Next i
        For i=0 To tmp
          If pos_orig(i)\x>0 And pos_orig(i)\y<n_ex
            DrawText(95+len_orig(i)/2+scaleplot_x*pos_orig(i)\x,342-scaleplot_y*pos_orig(i)\y,StrD((i-200)*interval_z,decimals_z),RGB(255,255,255))
          EndIf
          If pos_resid(i)\x>0 And pos_resid(i)\y<n_ex
            DrawText(145+len_resid(i)/2+scaleplot_x*(n_em+pos_resid(i)\x),342-scaleplot_y*pos_resid(i)\y,StrD((i-200)*interval_z,decimals_z),RGB(255,255,255))
          EndIf 
        Next i
        DrawingMode(#PB_2DDrawing_Transparent)
        For i=0 To tmp
          If pos_orig(i)\x>0 And pos_orig(i)\y<n_ex
            DrawText(95+len_orig(i)/2+scaleplot_x*pos_orig(i)\x,342-scaleplot_y*pos_orig(i)\y,StrD((i-200)*interval_z,decimals_z),RGB(0,0,0))
          EndIf
          If pos_resid(i)\x>0 And pos_resid(i)\y<n_ex
            DrawText(145+len_resid(i)/2+scaleplot_x*(n_em+pos_resid(i)\x),342-scaleplot_y*pos_resid(i)\y,StrD((i-200)*interval_z,decimals_z),RGB(0,0,0))
          EndIf 
        Next i
        DrawingMode(#PB_2DDrawing_Default)
      EndIf  
    EndIf
    
    ;Scale under 2D-Plots
    scalebars_and_legend(max,min,1) 
    
    If GetGadgetState(#CheckBox_component_position)=#PB_Checkbox_Checked And Not GetGadgetState(#Checkbox_contour)=#PB_Checkbox_Checked
      DrawingMode(#PB_2DDrawing_Transparent)
      For i=0 To n_comp
        DrawText(coord(i)\x*scaleplot_x+100,340-coord(i)\y*scaleplot_y,Str(i+1),RGB(255,255,255))
      Next i
      DrawingMode(#PB_2DDrawing_Default)
    EndIf
    max=0
    maxtemp.d
    If GetGadgetState(#CheckBox_relative_contents)=#PB_Checkbox_Checked
      For i=0 To n_comp
        max+A(drawnumber,i)
      Next i
    Else    
      max=-Pow(10,30)
      For i=0 To n_samp
        maxtemp=0
        For j=0 To n_comp
          maxtemp+A(i,j)
        Next j
        If maxtemp>max:max=maxtemp:EndIf
      Next i  
    EndIf
    
    lastx.i=40
    Select kindofdev
      Case 0
        For i=0 To n_comp
          Box(lastx,470,Round(A(drawnumber,i)/max*1200,#PB_Round_Nearest),20,farbe(i))
          Box(lastx,477,Round(A_dev(drawnumber,i)/max*1200*scaledev,#PB_Round_Nearest),6,lichte_farbe(i))
          lastx+Round(A(drawnumber,i)/max*1200,#PB_Round_Nearest)
          Box(lastx-Round(A_dev(drawnumber,i)/max*1200*scaledev,#PB_Round_Nearest),477,Round(A_dev(drawnumber,i)/max*1200*scaledev,#PB_Round_Nearest),6,lichte_farbe(i))
        Next i 
      Case 1
        For i=0 To n_comp
          Box(lastx,470,Round(A(drawnumber,i)/max*1200,#PB_Round_Nearest),20,farbe(i))
          Box(lastx,477,Round(A_se(drawnumber,i)/max*1200*scaledev,#PB_Round_Nearest),6,lichte_farbe(i))
          lastx+Round(A(drawnumber,i)/max*1200,#PB_Round_Nearest)
          Box(lastx-Round(A_se(drawnumber,i)/max*1200*scaledev,#PB_Round_Nearest),477,Round(A_se(drawnumber,i)/max*1200*scaledev,#PB_Round_Nearest),6,lichte_farbe(i))
        Next i 
      Case 2  
        For i=0 To n_comp
          Box(lastx,470,Round(A(drawnumber,i)/max*1200,#PB_Round_Nearest),20,farbe(i))
          Box(lastx,477,Round(A_sd(drawnumber,i)/max*1200*scaledev,#PB_Round_Nearest),6,lichte_farbe(i))
          lastx+Round(A(drawnumber,i)/max*1200,#PB_Round_Nearest)
          Box(lastx-Round(A_sd(drawnumber,i)/max*1200*scaledev,#PB_Round_Nearest),477,Round(A_sd(drawnumber,i)/max*1200*scaledev,#PB_Round_Nearest),6,lichte_farbe(i))
        Next i 
    EndSelect     
    DrawText(40,510,"current RMSE/NRMSE/CV(RMSE): "+StrD(rmse)+" / "+StrD(nrmse)+" / "+StrD(cvrmse))
    DrawText(640,510,"calculation time: "+Str(calculationtime)+"ms, HIX: "+StrD(hix(drawnumber)))
 
  EndIf
  DrawText(640,530,"Core: "+StrD(corecons,4)+"  exp. Var.: "+StrD(explainedv,4)+" dev:"+StrD(dev_rmse)+" Lackoffit:"+StrD(lackoffit))
  StopDrawing()  
;   If Not IsScreenActive()
;     
;     OpenWindowedScreen(WindowID(#Window_plot),0,0,breite,530,0,0,0)
;     !fldcw [v_FPU_ControlWord]
;   EndIf
  
    ClearScreen(RGB(255, 255, 255))
    StartDrawing(ScreenOutput())
  DrawingMode(#PB_2DDrawing_Default)
  DrawImage(ImageID(bildchen),0,0) 
  FreeImage(bildchen)
  StopDrawing()
  
  FlipBuffers()

EndProcedure

Procedure set_loadings()
  
  For i=0 To n_comp
    For j=0 To n_samp
      A(j,i)=1
      ;last_A(j,i)=1
    Next j
    If Not fixed(i)
      randommean.d=(scale_em(n_em)-scale_em(0))*Random(1000)/1000
      randomsigma.d=Random(1000)/4000*(scale_em(n_em)-scale_em(0))
      stepsize.d=scale_em(1)-scale_em(0)
      For j=0 To n_em
        B(j,i)=Exp(-Pow(j*stepsize-randommean,2)/(2*Pow(randomsigma,2)));Exp(-Pow((j-(i+1)*n_uv/(n_comp+2)),2)/Pow(n_uv/sigma_loadings,2));Random(1000)/1000;absorb(count,j);
        ;B(j,i)=Exp(-Pow(0.1*(j-(i+1)*n_em/(n_comp+2)),2))
        ;last_B(j,i)=Exp(-Pow(0.1*(j-(i+1)*n_em/(n_comp+2)),2))
      Next j
      randommean.d=(scale_ex(n_ex)-scale_ex(0))*Random(1000)/1000
      randomsigma.d=Random(1000)/4000*(scale_ex(n_ex)-scale_ex(0))
      stepsize.d=scale_ex(1)-scale_ex(0)
      For j=0 To n_ex
        C(j,i)=Exp(-Pow(j*stepsize-randommean,2)/(2*Pow(randomsigma,2)))
        ;C(j,i)=Exp(-Pow(0.1*(j-(i+1)*n_ex/(n_comp+2)),2))
        ;last_C(j,i)=Exp(-Pow(0.1*(j-(i+1)*n_ex/(n_comp+2)),2))
      Next j
    EndIf
  Next i 
  
EndProcedure  

Procedure import_student_t()
  
  temp.s
  
  temp=maindirectory+"student_t.txt"
  If ReadFile(0,temp)
    filefound=1
  EndIf   
  
  If filefound  
    n_import=0
    Repeat
      temp=ReadString(0)    
      CountString(temp, Chr(9)) 
      If ValD(StringField(temp,1,Chr(9)))
        n_import=n_import+1      
        ReDim student_t_verteilung.xy(n_import)
        student_t_verteilung(n_import)\x=ValD(StringField(temp,1,Chr(9)))
        student_t_verteilung(n_import)\y=ValD(StringField(temp,2,Chr(9)))        
      EndIf
    Until Eof(0)  
    CloseFile(0)
    If n_import
      n_student_t=n_import
    EndIf 
  Else
    MessageRequester("Warning","Student-t not found")
  EndIf
  
EndProcedure  

Procedure.d student_t(n.l)
  
  
  For i=1 To n_student_t
    If student_t_verteilung(i)\x>=n
      ProcedureReturn student_t_verteilung(i)\y
    EndIf  
  Next i
  
EndProcedure  

Procedure.d getcore(Array core.d(3))
  
  ;Berechnung des Tucker3 cores nach Lundy and Harshman 1989, Berechnung der Core Consistency nach Kompany-Zareh et al 2012
  
  Dim pseudoA.d(n_comp,n_samp)
  Dim pseudoB.d(n_comp,n_em)
  Dim pseudoC.d(n_comp,n_ex)
  pseudoinverse(A(),pseudoA())
  pseudoinverse(B(),pseudoB())
  pseudoinverse(C(),pseudoC())
  
  max.d
  max1.d
  max2.d
  For rr=0 To n_comp:For s=0 To n_comp:For t=0 To n_comp
    max2=0
    For k=0 To n_ex
      max1=0
      event=WindowEvent()
      While event
        n_events+1
        ReDim events.eventstructure(n_events)
        events(n_events)\event=event
        events(n_events)\eventtype=EventType()
        events(n_events)\eventgadget=EventGadget()
        event=WindowEvent()
      Wend
      For j=0 To n_em
        max=0
        For i=0 To n_samp
          max+pseudoA(rr,i)*X(i,j,k)
        Next i
        max1+pseudoB(s,j)*max
      Next j
      max2+pseudoC(t,k)*max1
    Next k
    core(rr,s,t)=max2
  Next t:Next s:Next rr 
  
  Dim t.d(n_comp,n_comp,n_comp)
  For i=0 To n_comp
    t(i,i,i)=1
  Next i
  max=0
  max1=0
  For d=0 To n_comp
    For e=0 To n_comp
      For f=0 To n_comp
        max+Pow(core(d,e,f)-t(d,e,f),2)
        max1+Pow(core(d,e,f),2)
      Next f  
    Next e  
  Next d
  max/max1
  ;max/n_comp
  ProcedureReturn 1-max

EndProcedure  

Procedure draw_scale_and_legend(min.d,max.d,drawlegend.b)
  
  scalerange_z.d=max-min
  startscale_z.d=Round(min/interval_z,#PB_Round_Up)*interval_z
  endscale_z.d= Round(max/interval_z,#PB_Round_Down)*interval_z   
  n_ticks_z.i=Round((endscale_z-startscale_z)/interval_z,#PB_Round_Nearest)
  
  ;scalerange_em.d=scale_em(n_em)-scale_em(0)
  ;startscale_em.d=Round(scale_em(0)/interval,#PB_Round_Up)*interval
  ;endscale_em.d=Round(scale_em(n_em)/interval,#PB_Round_Down)*interval 
  ;n_ticks_em.i=Round((endscale_em-startscale_em)/interval,#PB_Round_Nearest)
  ;scalerange_ex.d=scale_ex(n_ex)-scale_ex(0)
  ;startscale_ex.d=Round(scale_ex(0)/interval,#PB_Round_Up)*interval
  ;endscale_ex.d=Round(scale_ex(n_ex)/interval,#PB_Round_Down)*interval 
  ;n_ticks_ex.i=Round((endscale_ex-startscale_ex)/interval,#PB_Round_Nearest)
  scaletext.s
  
  
    For i=0 To n_ticks_em
      xpoint.i=100+((startscale_em+i*interval)-scale_em(0))/(scalerange_em)*n_em*scaleplot_x
      LineXY(xpoint,(n_ex+1)*scaleplot_y+10,xpoint,(n_ex+1)*scaleplot_y+12,RGB(0,0,0))
      scaletext=StrD(startscale_em+i*interval,decimals)    
      DrawText(xpoint-Len(scaletext)*4,(n_ex+1)*scaleplot_y+20,scaletext,RGB(0,0,0),RGB(255,255,255))
    Next i
    For i=0 To n_ticks_ex
      ypoint.i=10+(n_ex+1)*scaleplot_y-((startscale_ex+i*interval)-scale_ex(0))/(scalerange_ex)*n_ex*scaleplot_y
      LineXY(98,ypoint,100,ypoint,RGB(0,0,0))
      scaletext=StrD(startscale_ex+i*interval,decimals)    
      DrawText(100-Len(scaletext)*10,ypoint-8,scaletext,RGB(0,0,0),RGB(255,255,255))
    Next i  
    
    mitte.i=(n_em*scaleplot_x-30)/2
    If mitte<0:mitte=0:EndIf
    DrawingFont(FontID(symbolfont))
    DrawText(100+mitte,(n_ex+1)*scaleplot_y+40,Chr(108),RGB(0,0,0),RGB(255,255,255))
    DrawingFont(#PB_Default)
    DrawText(112+mitte,(n_ex+1)*scaleplot_y+50,"em",RGB(0,0,0),RGB(255,255,255))
    DrawText(140+mitte,(n_ex+1)*scaleplot_y+48,"[nm]",RGB(0,0,0),RGB(255,255,255))
    mitte.i=(n_ex*scaleplot_y-30)/2
    If mitte<0:mitte=0:EndIf
    DrawingFont(FontID(symbolfont))
    DrawRotatedText(60-Len(scaletext)*10,(n_ex+1)*scaleplot_y-mitte+30,Chr(108),90,RGB(0,0,0))
    DrawingFont(#PB_Default)
    DrawRotatedText(70-Len(scaletext)*10,(n_ex+1)*scaleplot_y-mitte-12+30,"ex",90,RGB(0,0,0))
    DrawRotatedText(68-Len(scaletext)*10,(n_ex+1)*scaleplot_y-mitte-40+30,"[nm]",90,RGB(0,0,0))
    
    If drawlegend  
      DrawingMode(#PB_2DDrawing_Transparent)
      last_y.i=365
      DrawText(n_em*scaleplot_x+150,(n_ex+1)*scaleplot_y+20,"raman units",RGB(0,0,0),RGB(255,255,255))      
      For i=0 To n_ticks_z
        ypoint.i=10+(n_ex+1)*scaleplot_y-Round(i/(n_ticks_z+1)*(n_ex+1)*scaleplot_y,#PB_Round_Nearest)
        height.i=Round((n_ex+1)*scaleplot_y/(n_ticks_z+1),#PB_Round_Up)
        Box(n_em*scaleplot_x+150,ypoint,50,-height,gradient(Round((startscale_z+i*(endscale_z-startscale_z)/(n_ticks_z))/max*1020,#PB_Round_Nearest)))
        If height>15
          ypoint-(height-15)/2
        EndIf         
        If last_y-ypoint>=15
          last_y=ypoint
          scaletext=StrD(startscale_z+i*interval_z,decimals_z)
          DrawText(n_em*scaleplot_x+210,ypoint-15,scaletext,RGB(0,0,0),RGB(255,255,255))
        EndIf  
        freier_platz+height
      Next i 
      DrawingMode(#PB_2DDrawing_Default)
     EndIf 
      
EndProcedure  

Procedure alpha_them(width.i,height.i)
  DrawingMode(#PB_2DDrawing_AlphaChannel)
  For i=0 To width-1
    For j=0 To height-1
      temp.l=Point(i,j)
      If Red(temp)=255 And Green(temp)=255 And Blue(temp)=255
        Plot(i,j,RGBA(255,255,255,0))
      EndIf  
    Next j  
  Next i
EndProcedure  

Procedure export_file()
  
  max.d
  ueks.i
  If GetGadgetState(#CheckBox_component_position)=#PB_Checkbox_Checked
    Dim coord.xy(n_comp)
    For i=0 To n_comp
      max=0
      For j=1 To n_em
        If B(j,i)>max:max=B(j,i):ueks=j:EndIf
      Next j
      coord(i)\x=ueks
      max=0
      For j=1 To n_ex
        If C(j,i)>max:max=C(j,i):ueks=j:EndIf
      Next j
      coord(i)\y=ueks
    Next i  
  EndIf  
  
  #export_genauigkeit=1
  temp.s
  overwrite.b=0
  abbruch.b=0
  Repeat 
    temp=PathRequester("Select Path:", directory)
    
    If temp
      
      If ExamineDirectory(0,temp,"*.*")
        gibtsschon=1
        FinishDirectory(0)
        If MessageRequester("Warnung","Dateien im Verzeichnis: "+temp+" wirklich überschreiben?",#PB_MessageRequester_YesNo)=#PB_MessageRequester_Yes
          overwrite=1
          ;DeleteDirectory(temp,"*.*",#PB_FileSystem_Recursive|#PB_FileSystem_Force)
        EndIf
      EndIf  
    EndIf
  Until overwrite Or temp=""
  If overwrite
    
    If Not gibtsschon:CreateDirectory(temp):EndIf
    corecons=getcore(core())
    OpenFile(0,temp+"component_core.txt")
    WriteStringN(0,"Core Consistency:"+Chr(9)+StrD(corecons))
    WriteStringN(0,"Explained Variance:"+Chr(9)+StrD(explainedv))
    WriteStringN(0,"Lack of Fit:"+Chr(9)+StrD(lackoffit))
    WriteStringN(0,"RMSdev:"+Chr(9)+StrD(dev_rmse))
    WriteStringN(0,"RMSE:"+Chr(9)+StrD(ssq))
    WriteStringN(0,"NRMSE:"+Chr(9)+StrD(nssq))
    WriteStringN(0,"CV(RMSE):"+Chr(9)+StrD(cvssq))
    WriteStringN(0,"")
    For i=0 To n_comp
      For j=0 To n_comp
        For k=0 To n_comp
          If k<n_comp
            WriteString(0,StrD(core(i,j,k))+Chr(9))
          Else
            WriteStringN(0,StrD(core(i,j,k)))
          EndIf  
        Next k
      Next j
      WriteStringN(0,"")
    Next i
    CloseFile(0)
    
    OpenFile(0,temp+"component_concentration.txt") 
    WriteString(0," "+Chr(9))
    For i=0 To n_comp
      WriteString(0,"comp. "+Str(i+1)+Chr(9))
    Next i
     For i=0 To n_comp
        WriteString(0,"SE comp. "+Str(i+1)+Chr(9))
      Next i
      For i=0 To n_comp
        WriteString(0,"SD comp. "+Str(i+1)+Chr(9))
      Next i
      For i=0 To n_comp
        WriteString(0,"CI comp. "+Str(i+1)+Chr(9))
      Next i
    WriteStringN(0,"RMSE"+Chr(9)+"NRMSE"+Chr(9)+"CV(RMSE)"+Chr(9)+"HIX")
    For i=0 To n_samp
      WriteString(0,files(i)+Chr(9))
      rmse=0
      min.d=Pow(10,30)
      max.d=-Pow(10,30)
      localmean=0
      For j=0 To n_em
        For k=0 To n_ex
          If X(i,j,k)<min:min=X(i,j,k):EndIf
          If X(i,j,k)>max:max=X(i,j,k):EndIf
          localmean+X(i,j,k)
          rmse+Pow(E(i,j,k),2)
        Next k  
      Next j
      localmean/((n_em+1)*(n_ex+1))
      rmse/((n_em+1)*(n_ex+1))
      localrange=max-min
      rmse=Sqr(rmse)
      nrmse=rmse/localrange
      cvrmse=rmse/localmean      
      
      For j=0 To n_comp
        WriteString(0,StrD(A(i,j))+Chr(9))
      Next j
      For j=0 To n_comp
        WriteString(0,StrD(A_se(i,j))+Chr(9))
      Next j
      For j=0 To n_comp
        WriteString(0,StrD(A_sd(i,j))+Chr(9))
      Next j  
      For j=0 To n_comp
        If j<n_comp
          WriteString(0,StrD(A_dev(i,j))+Chr(9))
        Else
          WriteStringN(0,StrD(A_dev(i,j))+Chr(9)+StrD(rmse)+Chr(9)+StrD(nrmse)+Chr(9)+StrD(cvrmse)+Chr(9)+StrD(hix(i)))
        EndIf  
      Next j

    Next i

    CloseFile(0)
    
    OpenFile(0,temp+"component_em_spectra.txt")
    WriteString(0," "+Chr(9))
    For i=0 To n_comp
      WriteString(0,"comp. "+Str(i+1)+Chr(9))
    Next i
     For i=0 To n_comp
        WriteString(0,"SE comp. "+Str(i+1)+Chr(9))
      Next i
      For i=0 To n_comp
        WriteString(0,"SD comp. "+Str(i+1)+Chr(9))
      Next i
      For i=0 To n_comp
        If i<n_comp
          WriteString(0,"CI comp. "+Str(i+1)+Chr(9))
        Else
          WriteStringN(0,"CI comp. "+Str(i+1))
        EndIf  
      Next i
    For i=0 To n_em
      WriteString(0,StrD(scale_em(i))+Chr(9))
      For j=0 To n_comp
        WriteString(0,StrD(B(i,j))+Chr(9))
      Next j
      For j=0 To n_comp
        WriteString(0,StrD(B_se(i,j))+Chr(9))
      Next j
      For j=0 To n_comp
        WriteString(0,StrD(B_sd(i,j))+Chr(9))
      Next j
      For j=0 To n_comp
        If j<n_comp
          WriteString(0,StrD(B_dev(i,j))+Chr(9))
        Else
          WriteStringN(0,StrD(B_dev(i,j)))
        EndIf  
      Next j
    Next i
    CloseFile(0)
    
    OpenFile(0,temp+"component_ex_spectra.txt")
    WriteString(0," "+Chr(9))
    For i=0 To n_comp
      WriteString(0,"comp. "+Str(i+1)+Chr(9))
    Next i
     For i=0 To n_comp
        WriteString(0,"SE comp. "+Str(i+1)+Chr(9))
      Next i
      For i=0 To n_comp
        WriteString(0,"SD comp. "+Str(i+1)+Chr(9))
      Next i
      For i=0 To n_comp
        If i<n_comp
          WriteString(0,"CI comp. "+Str(i+1)+Chr(9))
        Else
          WriteStringN(0,"CI comp. "+Str(i+1))
        EndIf  
      Next i
    For i=0 To n_ex
      WriteString(0,StrD(scale_ex(i))+Chr(9))
      For j=0 To n_comp
        WriteString(0,StrD(C(i,j))+Chr(9))
      Next j
      For j=0 To n_comp
        WriteString(0,StrD(C_se(i,j))+Chr(9))
      Next j
      For j=0 To n_comp
        WriteString(0,StrD(C_sd(i,j))+Chr(9))
      Next j
      For j=0 To n_comp
        If j<n_comp
          WriteString(0,StrD(C_dev(i,j))+Chr(9))
        Else
          WriteStringN(0,StrD(C_dev(i,j)))
        EndIf  
      Next j
    Next i
    CloseFile(0) 
    
    Dim original.i(n_em,n_ex)
    Dim recon.i(n_em,n_ex)
    Dim resid.i(n_em,n_ex)
    
    For i=0 To n_comp
      CreateImage(0,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,32)
      StartDrawing(ImageOutput(0))
      Box(0,0,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,RGB(255,255,255))
      DrawingMode(#PB_2DDrawing_Default) 
      For j=0 To n_em
        For k=0 To n_ex
          original(j,k)=Round(comps(j,k,i)*1020,#PB_Round_Nearest)
          Punkt(j*scaleplot_x+100,10+(n_ex+1-k)*scaleplot_y,scaleplot_x,scaleplot_y,gradient(original(j,k)))
        Next k
      Next j
      draw_scale_and_legend(0,1,0)
      alpha_them(n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81)
      StopDrawing()
      SaveImage(0,temp+"component "+Str(i+1)+".png", #PB_ImagePlugin_PNG)      
      FreeImage(0)
    Next i    
    
    If Not GetGadgetState(#CheckBox_relative_contents)
      max.d=-Pow(10,30)
      min.d=Pow(10,30)
      For k=0 To n_samp
        For i=0 To n_em
          For j=0 To n_ex
            ;If X_origin(k,i,j)>max:max=X_origin(k,i,j):EndIf
            If X(k,i,j)>max:max=X(k,i,j):EndIf
            If X_recon(k,i,j)>max:max=X_recon(k,i,j):EndIf
            If E(k,i,j)>max:max=E(k,i,j):EndIf
            If E(k,i,j)<min:min=E(k,i,j):EndIf
          Next j  
        Next i
      Next k
    EndIf  
    If MessageRequester("Info","Extended Export?",#PB_MessageRequester_YesNo)=#PB_MessageRequester_Yes ;mini Or generic
      If MessageRequester("Warning","This will create "+Str(n_samp+1)+" additional subfolders",#PB_MessageRequester_YesNo)=#PB_MessageRequester_Yes
    For k=0 To n_samp
      
      CreateDirectory(temp+files(k)+"\")
      OpenFile(0,temp+files(k)+"\"+"original.txt")
      OpenFile(1,temp+files(k)+"\"+"reconstructed.txt")
      OpenFile(2,temp+files(k)+"\"+"residual.txt")
      
      If GetGadgetState(#CheckBox_relative_contents)
        max.d=-Pow(10,30)
        min.d=Pow(10,30)
        For i=0 To n_em
          For j=0 To n_ex
            ;If X_origin(k,i,j)>max:max=X_origin(k,i,j):EndIf
            If X(k,i,j)>max:max=X(k,i,j):EndIf
            If X_recon(k,i,j)>max:max=X_recon(k,i,j):EndIf
            If E(k,i,j)>max:max=E(k,i,j):EndIf
            If E(k,i,j)<min:min=E(k,i,j):EndIf
          Next j  
        Next i
      EndIf  
      
      For i=0 To n_em
        For j=0 To n_ex
          original(i,j)=Round(X_origin(k,i,j)/max*1020,#PB_Round_Nearest)
          recon(i,j)=Round(X_recon(k,i,j)/max*1020,#PB_Round_Nearest)
          resid(i,j)=Round(E(k,i,j)/max*1020,#PB_Round_Nearest)
        Next j  
      Next i
      CreateImage(0,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,32)
      CreateImage(1,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,32)
      CreateImage(2,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,32)
      CreateImage(3,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,32)
      CreateImage(4,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,32)
      CreateImage(5,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,32)
      StartDrawing(ImageOutput(0))
      Box(0,0,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,RGB(255,255,255))
      DrawingMode(#PB_2DDrawing_Default) 
      For i=0 To n_em
        For j=0 To n_ex
          Punkt(i*scaleplot_x+100,10+(n_ex+1-j)*scaleplot_y,scaleplot_x,scaleplot_y,gradient(original(i,j)))
        Next j  
      Next i
      draw_scale_and_legend(min,max,1)
      alpha_them(n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81)
      StopDrawing() 
 
      StartDrawing(ImageOutput(1))
      Box(0,0,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,RGB(255,255,255))
      DrawingMode(#PB_2DDrawing_Default) 
      For i=0 To n_em
        For j=0 To n_ex
          Punkt(i*scaleplot_x+100,10+(n_ex+1-j)*scaleplot_y,scaleplot_x,scaleplot_y,gradient(recon(i,j)))
        Next j  
      Next i
      draw_scale_and_legend(min,max,1)
      If GetGadgetState(#CheckBox_component_position)=#PB_Checkbox_Checked
        DrawingMode(#PB_2DDrawing_Transparent)
        For i=0 To n_comp
          DrawText(coord(i)\x*scaleplot_x+100,(n_ex+1-coord(i)\y)*scaleplot_y,Str(i+1),RGB(255,255,255))
        Next i
        DrawingMode(#PB_2DDrawing_Default)
      EndIf
      alpha_them(n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81)
      StopDrawing() 
      StartDrawing(ImageOutput(2))
      Box(0,0,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,RGB(255,255,255))
      DrawingMode(#PB_2DDrawing_Default) 
      For i=0 To n_em
        For j=0 To n_ex
          Punkt(i*scaleplot_x+100,10+(n_ex+1-j)*scaleplot_y,scaleplot_x,scaleplot_y,gradient(resid(i,j)))
        Next j  
      Next i
      draw_scale_and_legend(min,max,1)
      alpha_them(n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81)
      StopDrawing()
      
      ;contour
      For i=0 To n_em
        For j=0 To n_ex          
          recon(i,j)=Round(X_recon(k,i,j)/interval_z,#PB_Round_Down):If recon(i,j)<-200:recon(i,j)=-200:EndIf
          original(i,j)=Round(X_origin(k,i,j)/interval_z,#PB_Round_Down):If original(i,j)<-200:original(i,j)=-200:EndIf
          resid(i,j)=Round(scaledev*E(k,i,j)/interval_z,#PB_Round_Down):If resid(i,j)<-200:resid(i,j)=-200:EndIf
        Next j
      Next i
      tmp.i=Round(max/interval_z,#PB_Round_Down)+200
      Dim pos_orig.xy(tmp)
      Dim pos_recon.xy(tmp)
      Dim pos_resid.xy(tmp)
      Dim len_orig.i(tmp)
      Dim len_recon.i(tmp)
      Dim len_resid.i(tmp)
      count_orig.i
      count_recon.i
      count_resid.i
      count_orig_inv.i
      count_recon_inv.i
      count_resid_inv.i
      For j=1 To n_ex
        count_orig=0
        count_orig_inv=0
        count_recon=0
        count_recon_inv=0
        count_resid=0
        count_resid_inv=0
        For i=n_em To 0 Step -1
          If original(i,j)+1=original(i,j-1)
            If original(i,j-1)+200<=tmp
              count_orig+1              
              If count_orig>len_orig(original(i,j-1)+200)
                pos_orig(original(i,j-1)+200)\x=i
                pos_orig(original(i,j-1)+200)\y=j
                len_orig(original(i,j-1)+200)=count_orig
              EndIf
            EndIf
          Else 
            count_orig=0
          EndIf
          If original(i,j-1)+1=original(i,j)
            If original(i,j)+200<=tmp
              count_orig_inv+1
              If count_orig_inv>len_orig(original(i,j)+200)
                pos_orig(original(i,j)+200)\x=i
                pos_orig(original(i,j)+200)\y=j
                len_orig(original(i,j)+200)=count_orig_inv
              EndIf
            EndIf  
          Else
            count_orig_inv=0
          EndIf          
          If recon(i,j)+1=recon(i,j-1)
            count_recon+1
            If count_recon>len_recon(recon(i,j-1)+200)
              pos_recon(recon(i,j-1)+200)\x=i 
              pos_recon(recon(i,j-1)+200)\y=j
              len_recon(recon(i,j-1)+200)=count_recon
            EndIf
          Else
            count_recon=0
          EndIf
          If recon(i,j-1)+1=recon(i,j)
            count_recon_inv+1
            If count_recon_inv>len_recon(recon(i,j)+200)
              pos_recon(recon(i,j)+200)\x=i
              pos_recon(recon(i,j)+200)\y=j
              len_recon(recon(i,j)+200)=count_recon_inv
            EndIf
          Else
            count_recon_inv=0
          EndIf 
          If resid(i,j)+1=resid(i,j-1)
            count_resid+1
          Else
            If count_resid>len_resid(resid(i,j-1)+200)
              len_resid(resid(i,j-1)+200)=count_resid
              pos_resid(resid(i,j-1)+200)\x=i
              pos_resid(resid(i,j-1)+200)\y=j
            EndIf
            count_resid=0
          EndIf
          If resid(i,j-1)+1=resid(i,j)
            count_resid_inv+1
          Else
            If count_resid_inv>len_resid(resid(i,j)+200)
              len_resid(resid(i,j)+200)=count_resid_inv
              pos_resid(resid(i,j)+200)\x=i
              pos_resid(resid(i,j)+200)\y=j
            EndIf
            count_resid_inv=0
          EndIf
        Next i  
      Next j  

      convert_to_contour(original(),n_em,n_ex)
      convert_to_contour(resid(),n_em,n_ex)      
      convert_to_contour(recon(),n_em,n_ex)
      
      StartDrawing(ImageOutput(3))
      Box(0,0,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,RGB(255,255,255))
      DrawingMode(#PB_2DDrawing_Default) 
      For i=0 To n_em
        For j=0 To n_ex
          Punkt(i*scaleplot_x+100,10+(n_ex+1-j)*scaleplot_y,scaleplot_x,scaleplot_y,contour(original(i,j)))
        Next j  
      Next i      
      For i=0 To tmp
        If pos_orig(i)\x>0 And pos_orig(i)\y<n_ex
          DrawText(95+len_orig(i)/2+scaleplot_x*pos_orig(i)\x,2+(n_ex+1-pos_orig(i)\y)*scaleplot_y,StrD((i-200)*interval_z,decimals_z),RGB(255,255,255),RGB(255,255,255))
        EndIf
      Next i
      DrawingMode(#PB_2DDrawing_Transparent)
      For i=0 To tmp
        If pos_orig(i)\x>0 And pos_orig(i)\y<n_ex
          DrawText(95+len_orig(i)/2+scaleplot_x*pos_orig(i)\x,2+(n_ex+1-pos_orig(i)\y)*scaleplot_y,StrD((i-200)*interval_z,decimals_z),RGB(0,0,0),RGB(255,255,255))
        EndIf 
      Next i
      DrawingMode(#PB_2DDrawing_Default)         
      draw_scale_and_legend(min,max,0)
      alpha_them(n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81)
      StopDrawing() 
      StartDrawing(ImageOutput(4))
      Box(0,0,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,RGB(255,255,255))
      DrawingMode(#PB_2DDrawing_Default) 
      For i=0 To n_em
        For j=0 To n_ex
          Punkt(i*scaleplot_x+100,10+(n_ex+1-j)*scaleplot_y,scaleplot_x,scaleplot_y,contour(recon(i,j)))
        Next j  
      Next i
      For i=0 To tmp
        If pos_recon(i)\x>0 And pos_recon(i)\y<n_ex
          DrawText(95+len_recon(i)/2+scaleplot_x*pos_recon(i)\x,2+(n_ex+1-pos_recon(i)\y)*scaleplot_y,StrD((i-200)*interval_z,decimals_z),RGB(255,255,255),RGB(255,255,255))
        EndIf
      Next i
      DrawingMode(#PB_2DDrawing_Transparent)
      For i=0 To tmp
        If pos_recon(i)\x>0 And pos_recon(i)\y<n_ex
          DrawText(95+len_recon(i)/2+scaleplot_x*pos_recon(i)\x,2+(n_ex+1-pos_recon(i)\y)*scaleplot_y,StrD((i-200)*interval_z,decimals_z),RGB(0,0,0),RGB(255,255,255))
        EndIf 
      Next i
      DrawingMode(#PB_2DDrawing_Default) 
      draw_scale_and_legend(min,max,0)
      alpha_them(n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81)
      StopDrawing()
      StartDrawing(ImageOutput(5))
      Box(0,0,n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81,RGB(255,255,255))
      DrawingMode(#PB_2DDrawing_Default) 
      For i=0 To n_em
        For j=0 To n_ex
          Punkt(i*scaleplot_x+100,10+(n_ex+1-j)*scaleplot_y,scaleplot_x,scaleplot_y,contour(resid(i,j)))
        Next j  
      Next i
      For i=0 To tmp
        If pos_resid(i)\x>0 And pos_resid(i)\y<n_ex
          DrawText(95+len_resid(i)/2+scaleplot_x*pos_resid(i)\x,2+(n_ex+1-pos_resid(i)\y)*scaleplot_y,StrD((i-200)*interval_z,decimals_z),RGB(255,255,255),RGB(255,255,255))
        EndIf
      Next i
      DrawingMode(#PB_2DDrawing_Transparent)
      For i=0 To tmp
        If pos_resid(i)\x>0 And pos_resid(i)\y<n_ex
          DrawText(95+len_resid(i)/2+scaleplot_x*pos_resid(i)\x,2+(n_ex+1-pos_resid(i)\y)*scaleplot_y,StrD((i-200)*interval_z,decimals_z),RGB(0,0,0),RGB(255,255,255))
        EndIf 
      Next i
      DrawingMode(#PB_2DDrawing_Default)       
      draw_scale_and_legend(min,max,0)
      alpha_them(n_em*scaleplot_x+301,(n_ex+1)*scaleplot_y+81)
      StopDrawing() 
      
      ;Box(0,0,640,480,RGB(255,255,255))
      SaveImage(0,temp+files(k)+"\"+"original.png", #PB_ImagePlugin_PNG)
      SaveImage(1,temp+files(k)+"\"+"reconstructed.png", #PB_ImagePlugin_PNG)
      SaveImage(2,temp+files(k)+"\"+"residual.png", #PB_ImagePlugin_PNG)
      SaveImage(3,temp+files(k)+"\"+"original_contour.png", #PB_ImagePlugin_PNG)
      SaveImage(4,temp+files(k)+"\"+"reconstructed_contour.png", #PB_ImagePlugin_PNG)
      SaveImage(5,temp+files(k)+"\"+"residual_contour.png", #PB_ImagePlugin_PNG)
      For j=0 To 5
        FreeImage(j)
      Next j
      
      WriteString(0,""+Chr(9))
      WriteString(1,""+Chr(9))
      WriteString(2,""+Chr(9))
      For i=0 To n_ex
        If i<n_ex
          WriteString(0,StrD(scale_ex(i))+Chr(9))
          WriteString(1,StrD(scale_ex(i))+Chr(9))
          WriteString(2,StrD(scale_ex(i))+Chr(9))
        Else
          WriteStringN(0,StrD(scale_ex(i)))
          WriteStringN(1,StrD(scale_ex(i)))
          WriteStringN(2,StrD(scale_ex(i)))
        EndIf  
      Next i
      For i=0 To n_em Step #export_genauigkeit
        WriteString(0,StrD(scale_em(i))+Chr(9))
        WriteString(1,StrD(scale_em(i))+Chr(9))
        WriteString(2,StrD(scale_em(i))+Chr(9))
        For j=0 To n_ex Step #export_genauigkeit
          If j<n_ex
            WriteString(0,StrD(X_origin(k,i,j))+Chr(9))
            WriteString(1,StrD(X_recon(k,i,j))+Chr(9))
            WriteString(2,StrD(E(k,i,j))+Chr(9))
          Else
            WriteStringN(0,StrD(X_origin(k,i,j)))
            WriteStringN(1,StrD(X_recon(k,i,j)))
            WriteStringN(2,StrD(E(k,i,j)))
          EndIf  
        Next j
      Next i
      CloseFile(0)
      CloseFile(1)
      CloseFile(2)       
    Next k
    EndIf
    EndIf
  EndIf
  
EndProcedure  

Procedure.b nnls(Array Systemmatrix.d(2),Array Systemmatrix2.d(2),Array Zielmatrix.d(2),Array KonfInt.d(2),Array SE.d(2),Array SD.d(2),x.i,y.i,z.i,mode.i)
  
  passive_comp.i
  max.d
  index.i
  alpha.d
  countx.i
  county.i
  iter.i
  ssr.d
  temp1.d
  temp2.d
  Dim s.d(0,0)
  Dim BTBp.d(0,0)
  Dim BTBp_inv.d(0,0)
  Dim BTxp.d(0,0)
  Dim BTB.d(n_comp,n_comp)
  Dim BTB_inv.d(n_comp,n_comp)
  
  For l=0 To n_comp
    For m=0 To n_comp
      temp1=0
      For i=0 To y
        temp1+Systemmatrix(i,l)*Systemmatrix(i,m)
      Next i
      temp2=0
      For j=0 To z
        temp2+Systemmatrix2(j,l)*Systemmatrix2(j,m)
      Next j
      BTB(l,m)=temp1*temp2
    Next m
  Next l  
  
  For k=0 To x
    event=WindowEvent()
    While event
      n_events+1
      ReDim events.eventstructure(n_events)
      events(n_events)\event=event
      events(n_events)\eventtype=EventType()
      events(n_events)\eventgadget=EventGadget()
      event=WindowEvent()
    Wend
    
    Dim BTx.d(n_comp,0) 
    Dim xx.d(n_comp,0)     
    For l=0 To n_comp
      temp1=0
      For i=0 To y
        For j=0 To z
          If mode=3
            temp1+Systemmatrix(i,l)*Systemmatrix2(j,l)*X(i,j,k)
          ElseIf mode=2
            temp1+Systemmatrix(i,l)*Systemmatrix2(j,l)*X(i,k,j)
          ElseIf mode=1
            temp1+Systemmatrix(i,l)*Systemmatrix2(j,l)*X(k,i,j)
          EndIf  
        Next j  
      Next i
      BTx(l,0)=temp1  
    Next l   
    If nonneg              
      Dim passive.b(n_comp)
      Dim w.d(n_comp,0)
      Dim temp.d(n_comp,0)    
      For i=0 To n_comp
        passive(i)=0
      Next i
      passive_comp=-1 
      
      matrix_multi(BTB(),xx(),temp(),0,0);:Debug "2":EndIf
      
      max=-Pow(10,30)
      index=-1
      
      For i=0 To n_comp
        w(i,0)=BTx(i,0)-temp(i,0)
        If w(i,0)>max:max=w(i,0):index=i:EndIf
      Next i 
      
      iter=0
      While passive_comp<n_comp And max>Pow(10,-8+iter/10)      
        iter+1
        passive(index)=1
        passive_comp+1
        FreeArray(BTBp())
        FreeArray(BTBp_inv())
        FreeArray(BTxp())
        FreeArray(s())
        Dim BTBp.d(passive_comp,passive_comp)
        Dim BTBp_inv.d(passive_comp,passive_comp)
        Dim BTxp.d(passive_comp,0)
        Dim s.d(passive_comp,0)
        countx=-1        
        For j=0 To n_comp
          county=-1
          If passive(j)
            countx+1
            For i=0 To n_comp 
              If passive(i)
                county+1
                BTBp(countx,county)=BTB(i,j)
                BTxp(countx,0)=BTx(j,0)
              EndIf  
            Next i  
          EndIf
        Next j 
        If Not matrix_inv(BTBp(),BTBp_inv()):ProcedureReturn 0:EndIf;:Debug "3":EndIf
        matrix_multi(BTBp_inv(),BTxp(),s(),0,0);:Debug "4":EndIf
        
        max=Pow(10,30)
        For i=0 To passive_comp
          If s(i,0)<max:max=s(i,0):EndIf
        Next i
        While max<=0
          alpha=Pow(10,30)
          countx=-1
          For i=0 To n_comp            
            If passive(i)
              countx+1
              If xx(i,0)/(xx(i,0)-s(countx,0))<alpha And s(countx,0)<=0:alpha=xx(i,0)/(xx(i,0)-s(countx,0)):EndIf
            EndIf  
          Next i
          countx=-1
          For i=0 To n_comp
            If passive(i)
              countx+1
              xx(i,0)-alpha*(xx(i,0)-s(countx,0))
            Else  
              xx(i,0)-alpha*(xx(i,0))
            EndIf  
          Next i
          For i=0 To n_comp
            If xx(i,0)<Pow(10,-8) And passive(i)
              passive(i)=0
              passive_comp-1            
            EndIf  
          Next i        
          FreeArray(BTBp())
          FreeArray(BTBp_inv())
          FreeArray(BTxp())
          FreeArray(s())
          Dim BTBp.d(passive_comp,passive_comp)
          Dim BTBp_inv.d(passive_comp,passive_comp)
          Dim BTxp.d(passive_comp,0)
          Dim s.d(passive_comp,0)
          countx=-1        
          For j=0 To n_comp
            county=-1
            If passive(j)
              countx+1
              For i=0 To n_comp 
                If passive(i)
                  county+1
                  BTBp(countx,county)=BTB(i,j)
                  BTxp(countx,0)=BTx(j,0)
                EndIf  
              Next i  
            EndIf
          Next j 
          If Not matrix_inv(BTBp(),BTBp_inv()):ProcedureReturn 0:EndIf;:Debug "3":EndIf
          matrix_multi(BTBp_inv(),BTxp(),s(),0,0);:Debug "4":EndIf
          max=Pow(10,30)
          For i=0 To passive_comp
            If s(i,0)<max:max=s(i,0):EndIf
          Next i              
        Wend  
        
        countx=-1
        For i=0 To n_comp
          If passive(i):
            countx+1
            xx(i,0)=s(countx,0)  
          EndIf
        Next         
        matrix_multi(BTB(),xx(),temp(),0,0);:Debug "2":EndIf     
        max=-Pow(10,30)
        index=-1
        For i=0 To n_comp
          w(i,0)=BTx(i,0)-temp(i,0)
          If w(i,0)>max And Not passive(i):max=w(i,0):index=i:EndIf
        Next i       
      Wend      
    Else
      matrix_inv(BTB(),BTB_inv())
      matrix_multi(BTB_inv(),BTx(),xx(),0,0)
    EndIf
    
    For i=0 To n_comp
      Zielmatrix(k,i)=xx(i,0)
    Next i  
    
    ssr=0
    For i=0 To y
      For j=0 To z
        temp1=0
        For l=0 To n_comp
          temp1+Systemmatrix(i,l)*Systemmatrix2(j,l)*Zielmatrix(k,l)
        Next l
        If mode=3
          ssr+Pow(temp1-X(i,j,k),2)
        ElseIf mode=2
          ssr+Pow(temp1-X(i,k,j),2)
        ElseIf mode=1
          ssr+Pow(temp1-X(k,i,j),2)
        EndIf  
      Next j  
    Next i 
    
    countx=-1
    If nonneg
      For i=0 To n_comp
        If passive(i)
          countx+1
          KonfInt(k,i)=Sqr(BTBp_inv(countx,countx)*ssr/((y+1)*(z+1)-(passive_comp+1)))*student_t((y+1)*(z+1)-(passive_comp+1))
          SE(k,i)=KonfInt(k,i)/student_t((y+1)*(z+1)-passive_comp)
          SD(k,i)=SE(k,i)*Sqr((y+1)*(z+1)-passive_comp)
        Else
          KonfInt(k,i)=0
          SE(k,i)=0
          SD(k,i)=0
        EndIf
      Next i
    Else
      For i=0 To n_comp
        KonfInt(k,i)=Sqr(BTB_inv(i,i)*ssr/((y+1)*(z+1)-(passive_comp+1)))*student_t((y+1)*(z+1)-(passive_comp+1))
        SE(k,i)=KonfInt(k,i)/student_t((y+1)*(z+1)-n_comp)
        SD(k,i)=SE(k,i)*Sqr((y+1)*(z+1)-n_comp)
      Next i
    EndIf
    
  Next k
  
  ProcedureReturn 1
  
EndProcedure  

Procedure removefix(Array aa.d(2),Array bb.d(2),Array cc.d(2),Array bdev.d(2),Array bse.d(2),Array bsd.d(2),Array cdev.d(2),Array cse.d(2),Array csd.d(2),Array an.d(2),Array bn.d(2),Array cn.d(2),Array bdevn.d(2),Array bsen.d(2),Array bsdn.d(2),Array cdevn.d(2),Array csen.d(2),Array csdn.d(2),remove.b)
  
  temp.d
  If remove
    count=-1
    For i=0 To n_comp
      If Not fixed(i)
        count+1
        For j=0 To n_em
          bn(j,count)=bb(j,i)
          bdevn(j,count)=bdev(j,i)
          bsen(j,count)=bse(j,i)
          bsdn(j,count)=bsd(j,i)
        Next j
        For j=0 To n_ex
          cn(j,count)=cc(j,i)
          cdevn(j,count)=cdev(j,i)
          csen(j,count)=cse(j,i)
          csdn(j,count)=csd(j,i)
        Next j  
        For j=0 To n_samp
          an(j,count)=aa(j,i)
        Next j  
      EndIf  
    Next i
        For i=0 To n_samp
          For j=0 To n_em
            For k=0 To n_ex
              temp=0
              For l=0 To n_fixed
                temp+aa(i,l)*bb(j,l)*cc(k,l)
              Next l  
              X(i,j,k)-temp
            Next k  
          Next j
        Next i 
  Else
    count=-1
    For i=0 To n_comp
      If Not fixed(i)
        count+1
        For j=0 To n_em
          bb(j,i)=bn(j,count)
          bdev(j,i)=bdevn(j,count)
          bse(j,i)=bsen(j,count)
          bsd(j,i)=bsdn(j,count)
        Next j
        For j=0 To n_ex
          cc(j,i)=cn(j,count)
          cdev(j,i)=cdevn(j,count)
          cse(j,i)=csen(j,count)
          csd(j,i)=csdn(j,count)
        Next j  
        For j=0 To n_samp
          aa(j,i)=an(j,count)
        Next j  
      Else
        For j=0 To n_em
          bdev(j,i)=0
          bse(j,i)=0
          bsd(j,i)=0
        Next j
        For j=0 To n_ex
          cdev(j,i)=0
          cse(j,i)=0
          csd(j,i)=0
        Next j  
      EndIf  
    Next i  
      For i=0 To n_samp
        For j=0 To n_em
          For k=0 To n_ex
            temp=0
            For l=0 To n_fixed
              temp+aa(i,l)*bb(j,l)*cc(k,l)
            Next l  
            X(i,j,k)+temp
          Next k  
        Next j
      Next i      
  EndIf  
  
EndProcedure  

Procedure about()
  
  text.s
  text+"PARAFAC software 3.1"+#eol+#eol
  text+"internal beta version belonging to the Chair of Hydrogeology, Friedrich-Schiller-University Jena, Germany"+#eol+#eol
  text+"###--DISCLAIMER--###"+#eol+#eol+"Without limitation of the foregoing, the developer of the provided software expressly does not warrant that:"+#eol
  text+"(a) the software will meet your requirements or expectations;"+#eol
  text+"(b) the software or the software content will be free of bugs, errors, viruses or other defects;"+#eol
  text+"(c) any results, output, or data provided through or generated by the software will be accurate, up-to-date, complete or reliable;"+#eol
  text+"(d) the software will be compatible with third party software;"+#eol
  text+"(e) any errors in the software will be corrected."+#eol+#eol
  text+"In no event shall the software developer be liable to you or any third parties for any special, punitive, incidental, indirect or consequential damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of use, lost data, or any liability, arising out of or in connection with the use of this software."+#eol
  text+#eol+#eol
  text+"references:"+#eol+#eol
  text+"Bro, R., (1997). PARAFAC. Tutorial and applications. Chemometrics and Intelligent Laboratory Systems (38), 149-171."+#eol
  text+"-"+#eol
  text+"Bro, R., De Jong, S., (1997). A fast non-negativity-constrained least squares algorithm. Journal of Chemometrics (11), 393-401."+#eol
  text+"-"+#eol
  text+"Bro, R., Kiers, H.A.L., (2003). A new efficient method for determining the number of components in PARAFAC models. Journal of Chemometrics (17), 274-286."+#eol
  text+"-"+#eol
  text+"Gauthier, T.D., Shane, E.C., Guerln, W.F., Seitz, W.R., Grant, C.L., (1986). Fluorescence Quenching Method For Determining Equilibrium Constants For Polycyclic Aromatic Hydrocarbons Binding To Dissolved Humic Materials. Environmental Science And Technology (20), 1162-1166."+#eol
  text+#eol+#eol+#eol
  text+"contact: Thomas.Ritschel@uni-jena.de"+#eol
  
  MessageRequester("About...",text)
  
  text=""

  If ReadFile(0,maindirectory+"changelog_PARAFAC.txt")
  
  temp.s=""
  Repeat
    temp=ReadString(0)
    If FindString(temp,"Version")
      temp=UCase(temp)
    EndIf  
    If temp And Not FindString(temp,"----") And Not FindString(temp,"====") And Not temp="References:"
      text+temp+#eol
    EndIf
  Until temp="References:" Or Eof(0)
  MessageRequester("Changelog",text)
  CloseFile(0)
  EndIf
  
EndProcedure  

Procedure shut_down()
  
If Not OpenPreferences("init.dat")
  CreatePreferences("init.dat")  
EndIf
WritePreferenceString("PARAFAC_defaultdirectory",directory)
WritePreferenceInteger("PARAFAC_decimals",decimals)
WritePreferenceDouble("PARAFAC_interval",interval)
WritePreferenceInteger("PARAFAC_decimals_z",decimals_z)
WritePreferenceDouble("PARAFAC_interval_z",interval_z)
WritePreferenceDouble("PARAFAC_plotscale_x",scaleplot_x)
WritePreferenceDouble("PARAFAC_plotscale_y",scaleplot_y)
WritePreferenceDouble("PARAFAC_devscale",scaledev)
WritePreferenceInteger("PARAFAC_kindofdev",kindofdev)
WritePreferenceInteger("PARAFAC_em_start",em_start)
WritePreferenceInteger("PARAFAC_ex_start",ex_start)
WritePreferenceInteger("PARAFAC_em_end",em_end)
WritePreferenceInteger("PARAFAC_ex_end",ex_end)
WritePreferenceInteger("PARAFAC_correct",correct)
WritePreferenceInteger("PARAFAC_minifiles",mini)
WritePreferenceInteger("PARAFAC_absorbance",absorbance)
WritePreferenceInteger("PARAFAC_generic",generic)
WritePreferenceInteger("PARAFAC_component_position",GetGadgetState(#CheckBox_component_position))
ClosePreferences()
End

EndProcedure  

Procedure.b one_iteration()
  
  max.d
  index.i=0
  count.i
  For i=0 To n_samp
    For j=0 To n_em
      For k=0 To n_ex
        X(i,j,k)=missing(j,k)*X_recon(i,j,k)+(1-missing(j,k))*X_origin(i,j,k)
      Next k
    Next j
  Next i  
  nnls(B(),C(),A(),A_dev(),A_se(),A_sd(),n_samp,n_em,n_ex,1)   ;fit A
  
  Dim B_nonfixed.d(n_em,n_comp-n_fixed-1)
  Dim A_nonfixed.d(n_samp,n_comp-n_fixed-1)
  Dim C_nonfixed.d(n_ex,n_comp-n_fixed-1)
  Dim B_dev_nonfixed.d(n_em,n_comp-n_fixed-1)
  Dim C_dev_nonfixed.d(n_ex,n_comp-n_fixed-1)
  Dim B_se_nonfixed.d(n_em,n_comp-n_fixed-1)
  Dim C_se_nonfixed.d(n_ex,n_comp-n_fixed-1)
  Dim B_sd_nonfixed.d(n_em,n_comp-n_fixed-1)
  Dim C_sd_nonfixed.d(n_ex,n_comp-n_fixed-1)
  
  If Not n_comp-(n_fixed+1)<0
    If Not generic   ;normalerweise wird ex und em gefixt
      removefix(A(),B(),C(),B_dev(),B_se(),B_sd(),C_dev(),C_se(),C_sd(),A_nonfixed(),B_nonfixed(),C_nonfixed(),B_dev_nonfixed(),B_se_nonfixed(),B_sd_nonfixed(),C_dev_nonfixed(),C_se_nonfixed(),C_sd_nonfixed(),1)
      n_comp-(n_fixed+1)
    If Not nnls(A_nonfixed(),C_nonfixed(),B_nonfixed(),B_dev_nonfixed(),B_se_nonfixed(),B_sd_nonfixed(),n_em,n_samp,n_ex,2):ProcedureReturn 0:EndIf
    ;nnls(A(),C(),B(),B_dev(),n_em,n_samp,n_ex,2) 
    For k=0 To n_comp
      max=0
      For j=0 To n_em
        If B_nonfixed(j,k)>max:max=B_nonfixed(j,k):EndIf
      Next j
      If max=0:max=1:EndIf
      For j=0 To n_em
        B_nonfixed(j,k)/max
        B_dev_nonfixed(j,k)/max
        B_se_nonfixed(j,k)/max
        B_sd_nonfixed(j,k)/max
      Next j
      For j=0 To n_samp
        A_nonfixed(j,k)*max
      Next j  
    Next k
  Else  ;bei generischen wird nur ein part gefixt
        If Not nnls(A(),C(),B(),B_dev(),B_se(),B_sd(),n_em,n_samp,n_ex,2):ProcedureReturn 0:EndIf
    ;nnls(A(),C(),B(),B_dev(),n_em,n_samp,n_ex,2) 
    For k=0 To n_comp
      max=0
      For j=0 To n_em
        If B(j,k)>max:max=B(j,k):EndIf
      Next j
      If max=0:max=1:EndIf
      For j=0 To n_em
        B(j,k)/max
        B_dev(j,k)/max
        B_se(j,k)/max
        B_sd(j,k)/max
      Next j
      For j=0 To n_samp
        A(j,k)*max
      Next j  
    Next k
      removefix(A(),B(),C(),B_dev(),B_se(),B_sd(),C_dev(),C_se(),C_sd(),A_nonfixed(),B_nonfixed(),C_nonfixed(),B_dev_nonfixed(),B_se_nonfixed(),B_sd_nonfixed(),C_dev_nonfixed(),C_se_nonfixed(),C_sd_nonfixed(),1)
      n_comp-(n_fixed+1)
    EndIf

    If Not nnls(A_nonfixed(),B_nonfixed(),C_nonfixed(),C_dev_nonfixed(),C_se_nonfixed(),C_sd_nonfixed(),n_ex,n_samp,n_em,3):ProcedureReturn 0:EndIf
    
    ;nnls(A(),B(),C(),C_dev(),n_ex,n_samp,n_em,3) 
    
    For k=0 To n_comp
      max=0
      For j=0 To n_ex
        If C_nonfixed(j,k)>max:max=C_nonfixed(j,k):EndIf
      Next j
      If max=0:max=1:EndIf
      For j=0 To n_ex
        C_nonfixed(j,k)/max
        C_dev_nonfixed(j,k)/max
        C_se_nonfixed(j,k)/max
        C_sd_nonfixed(j,k)/max
      Next j
      For j=0 To n_samp
        A_nonfixed(j,k)*max
      Next j
    Next k    
    n_comp+(n_fixed+1)
    removefix(A(),B(),C(),B_dev(),B_se(),B_sd(),C_dev(),C_se(),C_sd(),A_nonfixed(),B_nonfixed(),C_nonfixed(),B_dev_nonfixed(),B_se_nonfixed(),B_sd_nonfixed(),C_dev_nonfixed(),C_se_nonfixed(),C_sd_nonfixed(),0)
  EndIf  
  
  If calccore
    corecons=getcore(core())
  EndIf
  
  For i=0 To n_comp
    max=0
    For j=0 To n_em
      If B(j,i)>max:max=B(j,i):EndIf
    Next j
    If max=0
      randommean.d=(scale_em(n_em)-scale_em(0))*Random(1000)/1000
      randomsigma.d=Random(1000)/4000*(scale_em(n_em)-scale_em(0))
      stepsize.d=scale_em(1)-scale_em(0)
      For j=0 To n_em
        B(j,i)=Exp(-Pow(j*stepsize-randommean,2)/(2*Pow(randomsigma,2)))
      Next j
    EndIf
    max=0
    For j=0 To n_ex
      If C(j,i)>max:max=C(j,i):EndIf
    Next j
    If max=0
      randommean.d=(scale_ex(n_ex)-scale_ex(0))*Random(1000)/1000
      randomsigma.d=Random(1000)/4000*(scale_ex(n_ex)-scale_ex(0))
      stepsize.d=scale_ex(1)-scale_ex(0)
      For j=0 To n_ex
        C(j,i)=Exp(-Pow(j*stepsize-randommean,2)/(2*Pow(randomsigma,2)))
      Next j
    EndIf
  Next i  
  
EndProcedure  

import_student_t()
Open_Window_0()
OpenWindowedScreen(WindowID(#Window_0),1,1,1,1,0,0,0)
If generic:generic_alt=0:Else:generic_alt=1:EndIf
Repeat
  event=WaitWindowEvent()
  ExamineKeyboard()
  If event=#PB_Event_CloseWindow Or KeyboardPushed(#PB_Key_Escape)
    End
  EndIf 
  generic=GetGadgetState(#Checkbox_generic)
  If Not generic=generic_alt
    generic_alt=generic
    If generic
      HideGadget(#Spin_em_start,1)
      HideGadget(#Spin_em_end,1)
      HideGadget(#Spin_ex_start,1)
      HideGadget(#Spin_ex_end,1)
      HideGadget(#Text_em_start,1)
      HideGadget(#Text_em_end,1)
      HideGadget(#Text_ex_start,1)
      HideGadget(#Text_ex_end,1)
      ;HideGadget(#Text_generic_start,0)
      ;HideGadget(#Text_generic_end,0)
    Else
      HideGadget(#Spin_em_start,0)
      HideGadget(#Spin_em_end,0)
      HideGadget(#Spin_ex_start,0)
      HideGadget(#Spin_ex_end,0)
      HideGadget(#Text_em_start,0)
      HideGadget(#Text_em_end,0)
      HideGadget(#Text_ex_start,0)
      HideGadget(#Text_ex_end,0)
      ;HideGadget(#Text_generic_start,1)
      ;HideGadget(#Text_generic_end,1)
    EndIf  
  EndIf  
Until EventGadget()=#Button_import_samples And EventType()=#PB_EventType_LeftClick
correct=GetGadgetState(#Checkbox_correct)
absorbance=GetGadgetState(#Checkbox_correctabs)
;mini=GetGadgetState(#Checkbox_mini_files)
em_start=GetGadgetState(#Spin_em_start)
ex_start=GetGadgetState(#Spin_ex_start)
ex_end=GetGadgetState(#Spin_ex_end)
em_end=GetGadgetState(#Spin_em_end)
generic=GetGadgetState(#Checkbox_generic)

CloseWindow(#Window_0)
If generic:import_file_generic():Else:import_file():EndIf
Open_Window_1()
OpenWindowedScreen(WindowID(#Window_1),1,1,1,1,0,0,0)

Repeat
  event=WaitWindowEvent()
  eventgadget=EventGadget()
  eventtype=EventType()
  ExamineKeyboard()
  n_comp=GetGadgetState(#Spin_n_comp)-1
  ;R=GetGadgetState(#Track_1)/1000
  ;SetGadgetText(#Text_2,StrD(R,2))
  If eventgadget=#Button_About And eventtype=#PB_EventType_LeftClick
    About()
  EndIf
  If (eventgadget=#Button_Beenden And eventtype=#PB_EventType_LeftClick) Or event=#PB_Event_CloseWindow Or KeyboardPushed(#PB_Key_Escape)
    If MessageRequester("Warnung","Wirklich Beenden?",#PB_MessageRequester_YesNo)=#PB_MessageRequester_Yes 
      shut_down()
    EndIf  
  EndIf  
  n_comp_alt=n_comp
Until EventGadget()=#Button_Start_PARAFAC And EventType()=#PB_EventType_LeftClick

;ReDim best_A.d(n_samp,n_comp)
ReDim A.d(n_samp,n_comp)
ReDim B.d(n_em,n_comp)
ReDim C.d(n_ex,n_comp)
ReDim A_dev.d(n_samp,n_comp)
ReDim B_dev.d(n_em,n_comp)
ReDim C_dev.d(n_ex,n_comp)
ReDim A_se.d(n_samp,n_comp)
ReDim B_se.d(n_em,n_comp)
ReDim C_se.d(n_ex,n_comp)
ReDim A_sd.d(n_samp,n_comp)
ReDim B_sd.d(n_em,n_comp)
ReDim C_sd.d(n_ex,n_comp)
ReDim fixed.i(n_comp)
ReDim comps.d(n_em,n_ex,n_comp)
;ReDim last_A.d(n_samp,n_comp)
;ReDim last_B.d(n_em,n_comp)
;ReDim last_C.d(n_ex,n_comp)
;ReDim best_B.d(n_em,n_comp)
;ReDim best_C.d(n_ex,n_comp)
;ReDim best_A_dev.d(n_samp,n_comp)
;ReDim best_B_dev.d(n_em,n_comp)
;ReDim best_C_dev.d(n_ex,n_comp)
For i=0 To n_comp
  AddGadgetItem(#ListGadget,2,"component "+Str(n_comp+1-i))
Next i 
set_loadings()
If scaleplot_y*(n_ex+1)>350
  scaleplot_y=300/(n_ex+1)
    SetGadgetText(#String_scaley,StrD(scaleplot_y))
  EndIf
  If scaleplot_x*(2*n_em+2)>1080
    scaleplot_x=1000/(2*n_em+2)
    SetGadgetText(#String_scalex,StrD(scaleplot_x))
  EndIf
  temp.d

  breite=65+3*n_em*scaleplot_x
  hoehe=350+n_ex*scaleplot_y
ExamineDesktops()
If breite<1280:breite=1280:EndIf
If hoehe<600:hoehe=600:EndIf
;If breite>DesktopWidth(0)-20:breite=DesktopWidth(0)-20:EndIf
;Debug DesktopWidth(0)
OpenWindow(#Window_plot,0,0,breite,hoehe,"Plot",#PB_Window_SystemMenu | #PB_Window_MinimizeGadget | #PB_Window_SizeGadget )
OpenWindowedScreen(WindowID(#Window_plot),0,0,breite,hoehe,0,0,0)
!fldcw [v_FPU_ControlWord]
draw_plot()

abbruch.i=0

Repeat
  If GetGadgetState(#Button_Pause):event=WaitWindowEvent():Else:event=WindowEvent():EndIf
  eventgadget=EventGadget()
  eventtype=EventType()
  While event Or n_events
    ExamineKeyboard()
    If KeyboardPushed(#PB_Key_Escape) Or event=#PB_Event_CloseWindow Or (eventgadget=#Button_Beenden And eventtype=#PB_EventType_LeftClick)
      If MessageRequester("Warnung","Wirklich Beenden?",#PB_MessageRequester_YesNo)=#PB_MessageRequester_Yes 
        Break(2)
      EndIf  
    EndIf
    If (eventgadget=#Button_Reset And eventtype=#PB_EventType_LeftClick)
      set_loadings()
      allesanders=1
      draw_plot()
    EndIf
    If (eventgadget=#Button_Import And eventtype=#PB_EventType_LeftClick)
      import_state()
      allesanders=1
      draw_plot()
    EndIf
    If (eventgadget=#Button_importfixed And eventtype=#PB_EventType_LeftClick)
      import_fixed_spectra()
      allesanders=1
      draw_plot()
    EndIf
    If (eventgadget=#Button_About And eventtype=#PB_EventType_LeftClick)
      About()
    EndIf
    If n_events>0
      event=events(n_events)\event      
      eventgadget=events(n_events)\eventgadget
      eventtype=events(n_events)\eventtype
      n_events-1
    Else
      event=WindowEvent()
      eventgadget=EventGadget()
      eventtype=EventType()      
    EndIf 
  Wend
  ;R=GetGadgetState(#Track_1)/1000
  grayscale=GetGadgetState(#Checkbox_grayscale)
  ;SetGadgetText(#Text_2,StrD(R,2))
  n_comp=GetGadgetState(#Spin_n_comp)-1
  If n_comp<n_fixed
    If MessageRequester("Warning","number of fixed components can not be larger than number of components"+Chr(13)+Chr(10)+"Do want to remove all fixed components to decrease the number of components?",#PB_MessageRequester_YesNo)=#PB_MessageRequester_Yes 
      n_fixed=-1
      For i=0 To n_comp
        fixed(i)=0
      Next i
    Else
      n_comp=n_fixed
      SetGadgetState(#Spin_n_comp,n_comp+1)
    EndIf  
  EndIf  
  If GetGadgetState(#Checkbox_nonnegative)=#PB_Checkbox_Checked:nonneg=1:Else:nonneg=0:EndIf
  If n_comp_alt<>n_comp Or nonneg<>nonneg_alt Or n_fixed<>n_fixed_alt
    drawnumber=GetGadgetState(#ListGadget)
    For i=0 To n_comp_alt
      RemoveGadgetItem(#ListGadget,2)
    Next i
    n_fixed_alt=n_fixed
    nonneg_alt=nonneg
    n_comp_alt=n_comp
    ;best_ssq=Pow(10,30)
    ;ReDim best_A.d(n_samp,n_comp)
    ReDim A.d(n_samp,n_comp)
    ReDim B.d(n_em,n_comp)
    ReDim C.d(n_ex,n_comp)
    ReDim A_dev.d(n_samp,n_comp)
    ReDim B_dev.d(n_em,n_comp)
    ReDim C_dev.d(n_ex,n_comp)
    ReDim A_se.d(n_samp,n_comp)
    ReDim B_se.d(n_em,n_comp)
    ReDim C_se.d(n_ex,n_comp)
    ReDim A_sd.d(n_samp,n_comp)
    ReDim B_sd.d(n_em,n_comp)
    ReDim C_sd.d(n_ex,n_comp)
    ReDim fixed.i(n_comp)
    ReDim comps.d(n_em,n_ex,n_comp)
    FreeArray(core())
    Global Dim core.d(n_comp,n_comp,n_comp)
    ;ReDim best_B.d(n_em,n_comp)
    ;ReDim best_C.d(n_ex,n_comp)
    ;ReDim last_A.d(n_samp,n_comp)
    ;ReDim last_B.d(n_em,n_comp)
    ;ReDim last_C.d(n_ex,n_comp)
    ;ReDim best_A_dev.d(n_samp,n_comp)
    ;ReDim best_B_dev.d(n_em,n_comp)
    ;ReDim best_C_dev.d(n_ex,n_comp)    
    For i=0 To n_comp
      If fixed(n_comp-i)
        If generic
          AddGadgetItem(#ListGadget,2,"component "+Str(n_comp+1-i)+" ("+StringField(generic_names,n_comp-i+2,separator)+")")
        Else
          AddGadgetItem(#ListGadget,2,"component "+Str(n_comp+1-i)+" (fixed)")
        EndIf  
      Else
        AddGadgetItem(#ListGadget,2,"component "+Str(n_comp+1-i))
      EndIf  
    Next i 
    SetGadgetState(#ListGadget,drawnumber)
    set_loadings()
    ;EndIf  
  EndIf
  scaledev=ValD(GetGadgetText(#String_deviation))
  interval=ValD(GetGadgetText(#String_interval))
  decimals=GetGadgetState(#Spin_decimals)
  interval_z=ValD(GetGadgetText(#String_interval_z))
  decimals_z=GetGadgetState(#Spin_decimals_z)
  scaleplot_x=ValD(GetGadgetText(#String_scalex))
  scaleplot_y=ValD(GetGadgetText(#String_scaley))
  If GetGadgetState(#Option_sesdci1)     
    kindofdev=0
  ElseIf GetGadgetState(#Option_sesdci2)
    kindofdev=1
  Else
    kindofdev=2
  EndIf 

If scaleplot_y*(n_ex+1)>350
  scaleplot_y=300/(n_ex+1)
    SetGadgetText(#String_scaley,StrD(scaleploty))
  EndIf
  If scaleplot_x*(2*n_em+2)>1080
    scaleplot_x=1000/(2*n_em+2)
    SetGadgetText(#String_scalex,StrD(scaleplot_x))
  EndIf
  scalerange_em=scale_em(n_em)-scale_em(0)
  startscale_em=Round(scale_em(0)/interval,#PB_Round_Up)*interval
  endscale_em=Round(scale_em(n_em)/interval,#PB_Round_Down)*interval 
  n_ticks_em=Round((endscale_em-startscale_em)/interval,#PB_Round_Nearest)
  scalerange_ex=scale_ex(n_ex)-scale_ex(0)
  startscale_ex=Round(scale_ex(0)/interval,#PB_Round_Up)*interval
  endscale_ex=Round(scale_ex(n_ex)/interval,#PB_Round_Down)*interval 
  n_ticks_ex=Round((endscale_ex-startscale_ex)/interval,#PB_Round_Nearest)
  ;scaleplot=GetGadgetState(#Spin_scale)
  drawnumber=GetGadgetState(#ListGadget)
  drawnumber-1-n_comp-2 
  calculationtime=ElapsedMilliseconds()
  calccore=GetGadgetState(#Checkbox_Core) 
  twoD_comps=GetGadgetState(#Checkbox_2D_comps) 
  If Not GetGadgetState(#Button_Pause)
    one_iteration():    ;fit C and B and A once in this order
    ;counter+1
    iteration+1
  EndIf
  If Not GetGadgetState(#Button_Pause) Or allesanders
  allesanders=0  
  ssq=0
  n=0
  rmse=0
  n1=0
  explainedv=0
  max.d=-Pow(10,30)
  min.d=Pow(10,30)
  localmean=0
  ;mean=0
  For i=0 To n_comp
    For j=0 To n_em
      For k=0 To n_ex
        comps(j,k,i)=B(j,i)*C(k,i)     
      Next k
    Next j 
  Next i
  For i=0 To n_samp    ;calculate new SSQ
    For j=0 To n_em
      For k=0 To n_ex         
        temp=0
        If Not calccore
          For l=0 To n_comp
            temp+A(i,l)*B(j,l)*C(k,l)
          Next l
        Else
          For l=0 To n_comp
            For m=0 To n_comp
              For n=0 To n_comp
                temp+A(i,l)*B(j,l)*C(k,l)*core(l,m,n)
              Next n
            Next m  
          Next l
        EndIf
        X_recon(i,j,k)=temp        
        E(i,j,k)=X(i,j,k)-temp
        If Not missing(j,k)
        explainedv+Pow(temp-mean,2)
        If i=drawnumber
          If min>X(i,j,k):min=X(i,j,k):EndIf
          If max<X(i,j,k):max=X(i,j,k):EndIf
          localmean+X(i,j,k)
          rmse+Pow(E(i,j,k),2)
          n1+1
        EndIf
        ;mean+temp
        ssq+Pow(E(i,j,k),2)
        n+1
        EndIf
      Next k  
    Next j  
  Next i
  ;mean/n
  localmean/n1
  Debug "explained var"
  Debug explainedv
  explainedv=explainedv/totvar
  Debug explainedv
  rmse=rmse/n1
  rmse=Sqr(rmse)
  localrange=max-min
  nrmse=rmse/localrange
  cvrmse=rmse/localmean
  lackoffit=Sqr(ssq/squaresum)
  ssq=ssq/n
  ssq=Sqr(ssq)
  nssq=ssq/range
  cvssq=ssq/mean
  ratio=ssq/old_ssq
  old_ssq=ssq
  temp=0
  n=0
  For i=0 To n_comp
    For j=0 To n_em      
      temp+Pow(B_se(j,i),2)   ;changed from dev to se 
    Next j
    For k=0 To n_ex
      temp+Pow(C_se(k,i),2)
    Next k  
    n+2
  Next i
  temp/n
  dev_rmse=Sqr(temp)
  ;If ssq<best_ssq
  ;  counter=0
  ;  CopyArray(A(),best_A())
  ;  CopyArray(B(),best_B())
  ;  CopyArray(C(),best_C())
  ;  CopyArray(A_dev(),best_A_dev())
  ;  CopyArray(B_dev(),best_B_dev())
  ;  CopyArray(C_dev(),best_C_dev())
  ;  best_ssq=ssq
  ;EndIf
  EndIf
  draw_plot()
  
ForEver

;If MessageRequester("finished","Export best fit?"+Chr(13)+Chr(10)+"(else last fit is exported)",#PB_MessageRequester_YesNo)=#PB_MessageRequester_Yes 
;  CopyArray(best_A(),A())
;  CopyArray(best_B(),B())
;  CopyArray(best_C(),C())
;  CopyArray(best_A_dev(),A_dev())
;  CopyArray(best_B_dev(),B_dev())
;  CopyArray(best_C_dev(),C_dev())
;EndIf

explainedv=0
ssq=0
count=0
For i=0 To n_comp
  For j=0 To n_em
    For k=0 To n_ex
      comps(j,k,i)=B(j,i)*C(k,i)     
    Next k
  Next j 
Next i
For i=0 To n_samp
  For j=0 To n_em
    For k=0 To n_ex
       If Not missing(j,k)
      temp=0
      For l=0 To n_comp
        temp+A(i,l)*B(j,l)*C(k,l)
      Next l
      X_recon(i,j,k)=temp
      explainedv+Pow(temp-mean,2)
      E(i,j,k)=X(i,j,k)-temp
      ssq+Pow(E(i,j,k),2)
      count+1
      EndIf
    Next k  
  Next j  
Next i
ssq/count
ssq=Sqr(ssq)
explainedv=explainedv/totvar

export_file()

shut_down()


