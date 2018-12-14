!#Comments beginning with '!#' are new comments added by Susan Colarullo in 2017, comments beginning with '!' are old comments
!#from Gerhard Kuhn, Gary Krammes, Lisa Miller, and other USGS employees who worked on the program prior to 2017.

!#Prior to 2017 edits, the program was run through fixcon.exe to fix FORTRAN77 comment statements and continuation lines. 
!#FIXCON replaces "C" at beginning of each comment line with a "!", removes line continuation characters in column 6, 
!#and appends "&" to the end of all continuation statements.

!#This program does not need common block (.cmn) files to compile, since all common block variables have been incorporated 
!#into module CommonVariables below.


Module CommonVariables

implicit none

!Define all parameters for hard-wired values.
real, parameter :: MinimumRRFInCFSForUsingRegularBankStorageLookupTable=4.75, &
     MinimumRRFInCFSForBankStorageToBeConsideredNotEqualTo0=0.05,AcreFeetCorrespondingTo1CFSOverOneDay=1.9835, &
     MonumentCreekStreamWidthVsFlowScalingFactor=7.6,MonumentCreekStreamWidthVsFlowExponent=0.4806, &
     FountainCreekStreamWidthVsFlowScalingFactor=6.0,FountainCreekStreamWidthVsFlowExponent=0.434

!Define parameters for maximum array dimensions.
integer, parameter :: MaximumNumberOfRRFReleases=300
integer, parameter :: NumberMonumentCreekSubreaches=20,NumberMonumentCreekSegments=10
integer, parameter :: NumberFountainCreekSubreaches=20,NumberFountainCreekSegments=10
integer, parameter :: NumberFountainCreekGages=10,NumberMonumentCreekGages=10,NumberTributaryGages=10
integer, parameter :: NumberOfDiversions=50 
integer, parameter :: NumberMonumentCreekDiversions=30,NumberFountainCreekDiversions=50
integer, parameter :: NumberNativeReturnFlowReleases=10,NumberReusableReturnFlowReleases=20
integer, parameter :: NumberReusableReturnFlowEntities=20
integer, parameter :: MaximumNumberOfLinesUserInput=5
integer, parameter :: MaximumNumberOfBankStorageRecoveryDays=61
integer, parameter :: NumberOfPossibleIOErrors=114



!Variables associated with cdates.cmn common blocks (character date variables used in transit-loss accounting program).
character :: InputDate*8,PreviousRunDate*8,ReleaseDate*10,ExchangeDate*10

!Variables associated with idates.cmn common blocks (integer date variables).
integer :: DayOfCurrentReleaseDate,MonthOfCurrentReleaseDate,YearOfCurrentReleaseDate
integer :: DayOfLastReleaseDate,MonthOfLastReleaseDate,YearOfLastReleaseDate
   
!Variables associated with files.cmn common blocks (input/output file names).
!#unit    filename            variable
!#----  --------            ---------
!# 59  Mon_low.bsfile.dat     BankStorageLookupTableForLowMonumentCreekFlowsFileUnit  
!# 60  both.bsfile.dat        BankStorageLookupTableFileUnit   
!# 61  WebIn.fil              WebInputFileUnit              
!# 62  TextOutputFileUnit
!# 63  last.WebIn.fil         LastInputFileUnit        
!# 64  back1.fil              BACK1FileUnit        
!# 65  back2.fil              BACK2FileUnit         
!# 66  acct.fil                    ACCTSFileUnit         
!# 67  both_recov.fil         BankStorageRecoveryFileUnit  
!# 68  reusable_sum.fil       ACCUMFileUnit                
!# 69  scratch1                    SCRATCH1FileUnit          
!# 70  scratch2                    SCRATCH2FileUnit             
!# 71  CSVOutputFileUnit
!# 79  Accts_in.fil         INACCTSFileUnit

integer :: BankStorageLookupTableForLowMonumentCreekFlowsFileUnit, BankStorageLookupTableFileUnit, &
     WebInputFileUnit,TextOutputFileUnit,LastInputFileUnit,BACK1FileUnit,BACK2FileUnit,ACCTSFileUnit, &
     BankStorageRecoveryFileUnit,ACCUMFileUnit,SCRATCH1FileUnit,SCRATCH2FileUnit,CSVOutputFileUnit,INACCTSFileUnit
data BankStorageLookupTableForLowMonumentCreekFlowsFileUnit, BankStorageLookupTableFileUnit, &
     WebInputFileUnit,TextOutputFileUnit,LastInputFileUnit,BACK1FileUnit,BACK2FileUnit,ACCTSFileUnit, &
     BankStorageRecoveryFileUnit,ACCUMFileUnit,SCRATCH1FileUnit,SCRATCH2FileUnit,CSVOutputFileUnit &
      /59,60,61,62,63,64,65,66,67,68,69,70,71/
data INACCTSFileUnit /79/     

!Variables associated with handling input/output (IO) errors so that the user knows how IO went wrong. 
integer iostatus
character(200) :: FileWithIOError

 
!Variables associated with flags.cmn common blocks (various 'flag' variables). 
logical :: FlagForFryArkDiversion,FlagForExchangeDiversion,FlagForRRFDiversion,FlagToComputeNetGainWater, &
     BalanceAccountFlag, ConvergenceFlag,FlagForScratchOutput,FlagForSubreachOrSegmentLevel
integer :: NumberSubreachesNativeDiversionsGTNativeFlow   
character(3), allocatable :: SubreachesNativeFlowLessThan0(:)
character(1), allocatable :: FlagThatTurnsOnBalancingAccount(:)
character(2), allocatable :: FlagRRFDiversionFromBalancingAccount(:)
character :: NameOfCreek*3

        
!Variables associated with iopaths.cmn common blocks (pathnames for location of input/output files).
character(200) :: InputFilePathname,OutputFilePathname,InputOrOutputFilename

!Variables associated with accts.cmn common blocks (variables for transmountain diversion accounts).
!Accounts variables for diversion (sale) of Colorado Springs FRY-ARK and
!transmountain return flows to various ditches along Fountain Creek.
real, allocatable :: AmountOfCSFryArkWaterAvailable(:),AmountOfCSExchangeWaterAvailable(:),AmountOfCSWaterAvailable(:), &
     AmountOfCSFryArkWaterUsed(:),AmountOfCSExchangeWaterUsed(:),AmountOfCSWaterUsed(:)

!Variables associated with dimens.cmn common blocks (variables used to set dimension values).
integer :: NumberOfFountainCreekGagingStations,NumberOfFountainCreekDiversionDitches,NumberOfFountainCreekNodes, &
     NumberOfFountainCreekSubreaches,NumberOfFountainCreekSegments
integer :: NumberOfMonumentCreekGagingStations,NumberOfMonumentCreekDiversionDitches,NumberOfMonumentCreekNodes, &
     NumberOfMonumentCreekSubreaches,NumberOfMonumentCreekSegments
integer :: NumberOfReusableReturnFlows,NumberOfReusableReturnFlowDiversions,TotalNumberOfNodes, &
     NumberOfTributaryGagingStations,NumberOfNativeReturnFlows,NumberOfReusableReturnFlowsEntered
data NumberOfMonumentCreekNodes,NumberOfFountainCreekNodes /15,19/    
data NumberOfReusableReturnFlowsEntered /MaximumNumberOfRRFReleases /
    
!Variables associated with fixdat.cmn comm blocks (various variables with fixed values).
real, allocatable :: MonumentCreekSubreachLength(:),MonumentCreekSegmentLength(:)
real, allocatable :: FountainCreekSubreachLength(:),FountainCreekSegmentLength(:)
real :: DownstreamDistanceOfNodes(40)
data DownstreamDistanceOfNodes /0.0, 1.4, 1.8, 2.8, 4.8, 6.3, 7.8, 13.3, 15.1, 18.3, 19.1, 21.4, 25.6, 26.3, 27.6, 28.2, &
     29.9, 32.3, 36.8, 38.1, 41.5, 42.3, 44.4, 47.6, 48.3, 51.3, 52.6, 53.8, 57.2,61.1, 63.6, 67.3, 75.9, 78.3, 6*0.0/

!Variables associated with locations.cmn common blocks (variables used to define the locations where inputs and outputs are along Monument and Fountain Creeks.).
integer, allocatable :: MonumentCreekGagingStationNode(:),FountainCreekGagingStationNode(:),TributaryGagingStationNode(:)
integer, allocatable :: NativeReturnFlowReleaseNode(:),ReusableReturnFlowDiversionNode(:),ReusableReturnFlowEntity(:)
integer, allocatable :: ReusableReturnFlowReleaseNode(:),ReusableReturnFlowDeliveryNode(:)
integer, allocatable :: MonumentCreekDiversionNode(:),FountainCreekDiversionNode(:)

!Variables associated with names.cmn common blocks (character name variables).
character(8), allocatable :: USGSGagingStationNumber_Mainstem(:), USGSGagingStationNumber_Tributary(:)
character(40), allocatable :: USGSGagingStationName_Mainstem(:),USGSGagingStationName_Tributary(:)
     
character(56), allocatable :: ReleasedNativeReturnFlowName(:),ReleasedReusableReturnFlowName(:), &
     DivertedReusableReturnFlowName(:)
character(26), allocatable :: DivertedNativeReturnFlowNameOnMonumentCreek(:), &
     DivertedNativeReturnFlowNameOnFountainCreek(:)
character(17), allocatable :: MonumentCreekSubreachDitchNumber(:), &
     FountainCreekSubreachDitchNumber(:)
character(73), allocatable :: UserCommentFromWebInput(:)
  
!Variables associated with q_gages.cmn common blocks (streamflow-gaging station discharges).
real, allocatable :: GagingStationDischarge_Mainstem(:),GagingStationDischarge_Tributary(:)
real, allocatable :: SubreachSumOfGagingStationDischarge_Tributary(:)
real, allocatable :: VariableHeldForScratchFileInputOutput(:)

!Variables associated with ret_flows.cmn common blocks (return-flow discharge variables for Monument and Fountain Creeks).
real, allocatable :: NativeReturnFlowDischarge(:),ReusableReturnFlowDischarge(:),ReusableDiversionDischarge(:), &
     TemporaryReusableDiversionDischarge(:)
real, allocatable :: RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(:), &
     FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(:),UpstreamReusableReturnFlowByRRFEntity(:,:), &
     DownstreamReusableReturnFlowByRRFEntity(:,:)  
real, allocatable :: SubreachSumOfRRFReleases(:),SubreachSumOfRRFDiversions(:),SubreachSumOfMeasuredNativeReturnFlows(:), &
     SubreachSumReusableFlowConvertedToNativeFlow(:)
real, allocatable :: SubreachBankStorageLossForEachRRFRelease(:,:),SubreachBankStorageGainForEachRRFRelease(:,:), &
     SubreachBankStorageReleasedDuringRecovery(:,:,:)
real :: SurrogateCSReleaseFromUpstreamReservoir,SurrogateWWTFReturnFlowRelease,SurrogateCSFryArkWWTFReturnFlowRelease, &
     SurrogateFutureCSWWTFReturnFlowRelease,SurrogateFortCarsonReturnFlowRelease,SurrogateNWRFReturnFlowReleaseReachingWWTF, &
     SurrogateNWRFFryArkReleaseReachingWWTF,SurrogateNWRFFutureReleaseReachingWWTF
data SurrogateCSReleaseFromUpstreamReservoir,SurrogateWWTFReturnFlowRelease,SurrogateCSFryArkWWTFReturnFlowRelease, &
     SurrogateFutureCSWWTFReturnFlowRelease,SurrogateFortCarsonReturnFlowRelease,SurrogateNWRFReturnFlowReleaseReachingWWTF, &
     SurrogateNWRFFryArkReleaseReachingWWTF,SurrogateNWRFFutureReleaseReachingWWTF /8*0.0/
 
!Variables associated with divrsns.cmn common blocks (variables associated with any type of diversion).
real, allocatable :: NativeFlowDiversionAlongMonumentCreek(:),NativeFlowDiversionAlongFountainCreek(:), &
     FryArkFlowDiversionAlongFountainCreek(:),ExchangeDiversion1AlongFountainCreek(:),CSRRFDiversion2AlongFountainCreek(:)
real, allocatable :: SubreachSumNativeFlowDiversionsAlongMonumentCreek(:),SegmentSumNativeFlowDiversionsAlongMonumentCreek(:)
real, allocatable :: SubreachSumNativeFlowDiversionsAlongFountainCreek(:),SubreachSumFryArkDiversionsAlongFountainCreek(:), &
     SubreachSumExchangeDiversionsAlongFountainCreek(:),SubreachSumReusedDiversionsAlongFountainCreek(:), &
     SegmentSumNativeFlowDiversionsAlongFountainCreek(:),SegmentSumFryArkDiversionsAlongFountainCreek(:), &
     SegmentSumExchangeDiversionsAlongFountainCreek(:),SegmentSumReusedDiversionsAlongFountainCreek(:)
real, allocatable :: TransitLossSavingsDiversion(:),SegmentTransitLossFromFromFountainCreekDiversion(:), &
     SubreachTransitLossFromFountainCreekDiversion(:)
real, allocatable :: UpstreamReleaseInFountainCreekNeededToDeliverFryArkReturnFlow(:), &
     UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow(:), &
     UpstreamReleaseInFountainCreekNeededToDeliverReusedReturnFlow(:)
real :: SumOfFryArkDiversionsForCurrentDay,SumOfNonFryArkDiversionsForCurrentDay

!Variables associated with lrecov.cmn common blocks (length of recovery periods for Monument and Fountain Creeks).
integer :: SubreachBankStorageRecoveryPeriodLengthInDays(33)
data SubreachBankStorageRecoveryPeriodLengthInDays /4,3,1,1,2,2,1,1,1,2,2,1,12,8,28,18*60/

!Variables associated with bsarry.cmn common blocks (bank-storage losses for selected native streamflows and reusable return-flows from 1 to 200 CFS for each subreach.).
real :: RRFBankStorageLossLookupTableForBothCreeks(6600,16)
real :: RRFBankStorageLossLookupTableMonumentCreekLowRRF(364,16)
real :: LowerRangeOfIntervalsForRRFBankStorageLossLookupTableEntry(364), &
     UpperRangeOfIntervalsForRRFBankStorageLossLookupTableEntry(364)

!Variables associated with ftn_calcvar.cmn common blocks (variables that are computed for Fountain Creek subreaches during transit-loss computations).
real, allocatable :: SubreachBankStorageGainAlongFountainCreek(:),SubreachBankStorageLossAlongFountainCreek(:), &
     SubreachChannelStorageGainAlongFountainCreek(:),SubreachChannelStorageLossAlongFountainCreek(:), &
     SubreachDownstreamNativeFlowAlongFountainCreek(:),SubreachDownstreamTotalRRFAlongFountainCreek(:), &
     SubreachEvaporationLossFountainCreek(:),SubreachGainOrLossNativeFlowAlongFountainCreek(:), &
     SubreachGainOrLossReusableFlowAlongFountainCreek_pct(:),SubreachGainOrLossAllReusableFlowAlongFountainCreek(:), &
     SubreachUpstreamNativeFlowAlongFountainCreek(:),SubreachUpstreamTotalRRFAlongFountainCreek(:), &
     SubreachGainOrLossNativeFlowAlongFountainCreek_pct(:)
  
real :: CSURRFDeliveryWithTransitLossSavings,CSURRFDeliveryWithoutTransitLossSavings
data CSURRFDeliveryWithTransitLossSavings,CSURRFDeliveryWithoutTransitLossSavings /2*0.0/

!Variables associated with mon_calcvar.cmn common blocks (variables that are computed for Monument Creek subreaches during transit-loss computations).
real, allocatable :: SubreachBankStorageGainAlongMonumentCreek(:),SubreachBankStorageLossAlongMounumentCreek(:), &
     SubreachChannelStorageGainAlongMonumentCreek(:),SubreachChannelStorageLossAlongMonumentCreek(:), &
     SubreachDownstreamNativeFlowAlongMonumentCreek(:),SubreachDownstreamTotalRRFAlongMonumentCreek(:), &
     SubreachEvaporationLossMonumentCreek(:),SubreachGainOrLossNativeFlowAlongMonumentCreek(:), &
     SubreachGainOrLossReusableFlowAlongMonumentCreek_pct(:),SubreachGainOrLossAllReusableFlowAlongMonumentCreek(:), &
     SubreachUpstreamNativeFlowAlongMonumentCreek(:),SubreachUpstreamTotalRRFAlongMonumentCreek(:), &
     SubreachGainOrLossNativeFlowAlongMonumentCreek_pct(:)

     
!Variables associated with_calcvar.cmn common blocks (variables that are computed for Fountain Creek subreaches during transit-loss computations).
real :: FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow, &
     TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ,SegmentNativeFlowGainOrLossPerMile, &
     PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest,SubreachBankStorageGainMinusLoss, &
     SubreachGagedTributaryFlow,SegmentGagedTributaryFlow,SubreachGagedMainstemFlow,SegmentGagedMainstemFlow, &
     SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow,SegmentGainOrLossInNativeFlowBetweenGagingStations, &
     SegmentUpstreamTotalNativeFlow,SegmentDownstreamTotalNativeFlow,SegmentUpstreamTotalGagedFlow, &
     SegmentDownstreamTotalGagedFlow,SegmentDifferenceComputedDownstreamQCurrentAndLastIteration, &
     TemporarySubreachDownstreamNativeFlow,TemporarySubreachDownstreamNativeFlow2,TemporarySubreachDownstreamTotalRRF, &
     TemporarySubreachDownstreamTotalRRF2,TemporarySubreachUpstreamNativeFlow,TemporarySubreachUpstreamNativeFlow2, &
     TemporarySubreachUpstreamTotalRRF,TemporarySubreachUpstreamTotalRRF2,SubreachChannelStorageLoss,SubreachChannelStorageGain


end module CommonVariables



!*****************************************************************************
program Main
!*****************************************************************************
!This is the 2017 update of the transit-loss accounting program for Monument and Fountain Creeks. It supercedes
!all previous versions.

!This is the new transit-loss accounting program for Monument and Fountain Creeks that was developed as part of a 
!USGS study during August 2004--April 2007. See following two URL's:
!http://co.water.usgs.gov/projects/BBW00/index.html
!http://cointernal.cr.usgs.gov/proposals/BBW00.html

!For additional information. Two USGS SIR reports are published for the study:
!http://pubs.usgs.gov/sir/2006/5184/  Monument Creek modeling
!http://pubs.usgs.gov/sir/2007/5028/  Monument and Fountain Creeks accounting program
 
!Two reports also were published for the original study for Fountain Creek:
!http://pubs.er.usgs.gov/usgspubs/wri/wri874119  Fountain Creek modeling
!http://pubs.er.usgs.gov/usgspubs/ofr/ofr97637   Fountain Creek accounting program
                                            
!Modifications prior to 2017:
! G. Krammes - 09/2007
!  This is version that allows input up to 4 significant figures.  Original
!  allowed 2 sig figure.  I have added .csv output file (all header and
!  formatting stripped).  I have added .txt extension to formatted report.
! G. Krammes - 04/16/2008
!  Update to increase dimension for return flow nodes to 200.  Was 100.

!Program was modified during 2009 by L.D. Miller and G. Kuhn to enable computations of bank storage loss and gain for each individual 
!reusable return flow entity, whereas the program previously only did thesecomputations for all reusable return flows treated as a 
!single entity. See USGS memorandum to the record dated nn/nn/2010 for more details.

!Program modified again 01/2010--03/2010 by G. Kuhn to enable computation of transit loss savings (Net Gain Water) and use of Balance 
!Account for nine ditches along Fountain Creek when a SE Exchange diversion is specified for any of these ditches. The eight ditches 
!are :Chilcotte, Liston & Love N., Liston & Love S., Talcott & Cotton, Tom Wanless, Sutherland, Toof & Harmon, and Burke; Grenview 
!ditch is included to use Balance Account, but is not included in computation of Net Gain Water. See USGS memorandum to the record
!dated nn/nn/2010 for more details.

!The revision for "Net Gain Water" was modified to a lesser extent 09/2011 to correct an error in the computation of NGW. The original 
!method was described by the Colo. Dept. of Water Resources water commissioner in Colorado Springs, but was later determined to be 
!somewhat in error. This error was corrected. G. Kuhn, 10/28/2011.

!Additional revisions January--April, 2012; G Kuhn.
!Some aspects of the capability to simplify changes in input data were disabled as a result of a major revision of the program during 
!2009 to enable computation of bank storage loss and gain individually for each reusable return flow entity.

!Data in the recovery file originally were maintained as a single value for all reusable return flows, but the program modification 
!made during 2009 required that data in the recovery file be maintained for each individual reusable return flow. The configuration 
!of reusable return flows in use during the 2009 revisions became fixed and changes in number or order of the return
!flows could not be made. An auxiliary Fortran program was developed that performs the following:

!1. Input a new set of reusable return flow entity names as specified by the Colorado Water Commissioners who operate the accounting program;

!2. Incorporate the new set of reusable return flow names into the daily data input file;

!3. Incorporate the new set of reusable return flow names into the "Recovery" file while maintaining the bank storage loss and gain 
!data for all existing return flows in the file;

!4. Incorporate the new set of reusable return flow names into the file that maintains the monthly and annual flow release volumes for 
!each individual return flow entity while maintaining this data for all existing return flows in the file.

!The auxiliary Fortran program includes the capability to include any number of "blank" return flow entries at any location in the data 
!stream, for addition of a new entity without operating the auxiliary program; however, the auxiliary program can be used easily at any 
!time whenever there is a need to change the configuration of the reusable return flow entities.

!To enable addition of new reusable return flow entities into the blank data fields in the daily input file, the transit loss accounting 
!program also was modified. Changes were made in those sections of the program that read the daily input data file, in sections that 
!store the data in internal arrays, and in the sections that output the transit loss computation results.

!The program was modified to provide more resolution for very small reusable return flows on Monument Creek.  Linear interpolation was 
!used to derive additional values for initial bank storage loss in the lookup tables for reusable return flows between 0.0 and 5.0 ft3/s. 
!Between 0.0 and 1.0 ft3/s, the interpolation was in 0.1 ft3/s increments, between 1.0 and 4.0 ft3/s, the interpolation was in 0.2 ft3/s 
!increments, and between 4.0 and 5.0 ft3/s, the interpolation was in 0.5 ft3/s increments.

!The interpolated values of initial bank storage loss were incorporated into an auxiliary set of lookup tables for use only along Monument 
!Creek and for reusable return flows that are less than or equal to 4.5 ft3/s. The accounting program was modified to (1) read and store 
!the data in the auxiliary table internally (just as is done with the original table) and (2) use the auxiliary lookup tables for reusable 
!return flows less than or equal to 4.5 ft3/s and use the original lookup tables for flows larger than that value.

!******************************************************************************************************************************************
!Definition of local variables [Here and in the subroutines, other variables are defined in external "INCLUDE" (*.cmn) files]:
!------------------------------------------------------------------------------------------------------------------------------------------
!INACCTSFileUnit = program file name for the external file "Accts_in.fil" that contains data to update the "purchased" and "used" accounts data
!RunOption = program option (1-4)--entered on command line.
!InputErrorFlag = an error-checking variable, primarily used on reading input data, used throughout program, but is never explicitly declared.
!NetRRFQInMonumentDischargedToFountain = see subroutine "monumnt_tl."
!NetNativeQInMonumentDischargedToFountain = see subroutine "monumnt_tl."
!CommandLineInput = information passed to program on command line, such as option #.
!******************************************************************************************************************************************
use CommonVariables

implicit none

integer :: RunOption,InputErrorFlag,InputFilePathnameLength,InputFilenameLength,FileUnitCode,inlen
real :: NetRRFQInMonumentDischargedToFountain,NetNativeQInMonumentDischargedToFountain
character :: CommandLineInput*1

!Define FilePathName as an allocatable scalar string variable with an undefined number of characters so that you can get rid of trailing blanks.
character(:), allocatable :: FilePathName

!Set pathnames of directory for input files and output reports.
InputFilePathname = '/cygdrive/c/inetpub/wwwroot/transitLossV7/model_F90/'
OutputFilePathname = '/cygdrive/c/inetpub/wwwroot/transitLossV7/out_F90/'
!InputFilePathname = ''
!OutputFilePathname = ''
!InputFilePathname = './'
!OutputFilePathname = './'

!Windows test
!All pathnames are relative to directory in which the fortran executable resides.
! InputFilePathname=''
! OutputFilePathname=''

!*****************************************************************************
!Read program option from command line.
!#getarg(pos, value) is an intrinsic fortran function that returns the pos-th (1st) argument of the input command line value CommandLineInput. 
!#Better to use the GET_COMMAND_ARGUMENT intrinsic function if using a Fortran 2003 or later compiler.
!*****************************************************************************
CommandLineInput = ''
!call GetArg(1,CommandLineInput)
call get_command_argument(1,CommandLineInput,inlen,iostatus)

!# Test for no model option entered.
IF (LEN_TRIM(CommandLineInput) <= 0)  then
     print *, '  '
     print *, "Error!  You must provide a run option of (1 - 4)!"
     write (TextOutputFileUnit,1010) 'In MAIN--invalid program option. Must include option 1 - 4.'
     InputErrorFlag = 1
     go to 9999
end if
!# convert input to numeric
read(CommandLineInput,*) RunOption

if (RunOption < 1 .OR. RunOption > 4 ) then
     print *, "Error!  You must provide a run option of (1 - 4)!"
     write (TextOutputFileUnit,1010) 'In MAIN--invalid program option. Must include option 1 - 4.'
     InputErrorFlag = 1
     go to 9999
end if


!Reserve memory for all dynamically allocated arrays and set initial values.
allocate (ReusableReturnFlowReleaseNode(MaximumNumberOfRRFReleases),ReusableReturnFlowDeliveryNode(MaximumNumberOfRRFReleases))
ReusableReturnFlowReleaseNode=0
ReusableReturnFlowDeliveryNode=0
   
!SubreachesNativeFlowLessThan0=''  

allocate (AmountOfCSFryArkWaterAvailable(NumberOfDiversions), &
     AmountOfCSExchangeWaterAvailable(NumberOfDiversions), &
     AmountOfCSWaterAvailable(NumberOfDiversions),AmountOfCSFryArkWaterUsed(NumberOfDiversions), &
     AmountOfCSExchangeWaterUsed(NumberOfDiversions),AmountOfCSWaterUsed(NumberOfDiversions))
allocate (MonumentCreekSubreachLength(NumberMonumentCreekSubreaches), &
     MonumentCreekSegmentLength(NumberMonumentCreekSegments))
allocate (FountainCreekSubreachLength(NumberFountainCreekSubreaches), &
     FountainCreekSegmentLength(NumberFountainCreekSegments))
MonumentCreekSubreachLength=0.0
MonumentCreekSegmentLength=0.0
FountainCreekSubreachLength=0.0
FountainCreekSegmentLength=0.0     
     
allocate (MonumentCreekGagingStationNode(NumberMonumentCreekGages), &
     FountainCreekGagingStationNode(NumberFountainCreekGages),TributaryGagingStationNode(NumberTributaryGages))
MonumentCreekGagingStationNode=0
FountainCreekGagingStationNode=0
TributaryGagingStationNode=0

allocate(MonumentCreekDiversionNode(NumberMonumentCreekDiversions), &
     FountainCreekDiversionNode(NumberFountainCreekDiversions))
MonumentCreekDiversionNode=0
FountainCreekDiversionNode=0

allocate(NativeReturnFlowReleaseNode(NumberNativeReturnFlowReleases), &
     ReusableReturnFlowDiversionNode(NumberReusableReturnFlowReleases),ReusableReturnFlowEntity(NumberReusableReturnFlowEntities))
NativeReturnFlowReleaseNode=0
ReusableReturnFlowDiversionNode=0
ReusableReturnFlowEntity=0

allocate (character(56) :: ReleasedNativeReturnFlowName(NumberNativeReturnFlowReleases), &
     ReleasedReusableReturnFlowName(MaximumNumberOfRRFReleases), &
     DivertedReusableReturnFlowName(NumberReusableReturnFlowReleases))
ReleasedNativeReturnFlowName=''
ReleasedReusableReturnFlowName=''
DivertedReusableReturnFlowName=''

allocate (character(26) :: DivertedNativeReturnFlowNameOnMonumentCreek(NumberMonumentCreekDiversions), &
     DivertedNativeReturnFlowNameOnFountainCreek(NumberFountainCreekDiversions))
allocate (character(17) :: MonumentCreekSubreachDitchNumber(NumberMonumentCreekDiversions), &
     FountainCreekSubreachDitchNumber(NumberFountainCreekDiversions))
DivertedNativeReturnFlowNameOnMonumentCreek=''
DivertedNativeReturnFlowNameOnFountainCreek=''
MonumentCreekSubreachDitchNumber=''
FountainCreekSubreachDitchNumber=''
!allocate (character(73) :: UserCommentFromWebInput(MaximumNumberOfLinesUserInput))

allocate(GagingStationDischarge_Mainstem(NumberFountainCreekGages+NumberMonumentCreekGages), &
     GagingStationDischarge_Tributary(NumberTributaryGages))
GagingStationDischarge_Mainstem=0.0
GagingStationDischarge_Tributary=0.0
allocate(SubreachSumOfGagingStationDischarge_Tributary(NumberMonumentCreekSubreaches+ &
     NumberFountainCreekSubreaches))
SubreachSumOfGagingStationDischarge_Tributary=0.0
allocate(VariableHeldForScratchFileInputOutput(MaximumNumberOfBankStorageRecoveryDays))
VariableHeldForScratchFileInputOutput=0.0

allocate(NativeReturnFlowDischarge(NumberNativeReturnFlowReleases), &
     ReusableReturnFlowDischarge(MaximumNumberOfRRFReleases), &
     ReusableDiversionDischarge(NumberReusableReturnFlowReleases), &
     TemporaryReusableDiversionDischarge(NumberReusableReturnFlowReleases))   
NativeReturnFlowDischarge=0.0
ReusableReturnFlowDischarge=0.0
ReusableDiversionDischarge=0.0
TemporaryReusableDiversionDischarge=0.0  

allocate (RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(MaximumNumberOfRRFReleases), &
     FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(MaximumNumberOfRRFReleases), &
     UpstreamReusableReturnFlowByRRFEntity(2*NumberReusableReturnFlowEntities,MaximumNumberOfRRFReleases), &
     DownstreamReusableReturnFlowByRRFEntity(2*NumberReusableReturnFlowEntities,MaximumNumberOfRRFReleases))  
RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions=0.0
FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions=0.0
UpstreamReusableReturnFlowByRRFEntity=0.0
DownstreamReusableReturnFlowByRRFEntity=0.0

allocate (SubreachSumOfRRFReleases(NumberMonumentCreekSubreaches+NumberFountainCreekSubreaches), &
     SubreachSumOfRRFDiversions(NumberMonumentCreekSubreaches+NumberFountainCreekSubreaches), &
     SubreachSumOfMeasuredNativeReturnFlows(NumberMonumentCreekSubreaches+NumberFountainCreekSubreaches), &
     SubreachSumReusableFlowConvertedToNativeFlow(NumberMonumentCreekSubreaches+NumberFountainCreekSubreaches))
SubreachSumOfRRFReleases=0.0
SubreachSumOfRRFDiversions=0.0
SubreachSumOfMeasuredNativeReturnFlows=0.0
SubreachSumReusableFlowConvertedToNativeFlow=0.0
 
allocate (SubreachBankStorageLossForEachRRFRelease(NumberMonumentCreekSubreaches+NumberFountainCreekSubreaches, &
     MaximumNumberOfRRFReleases),SubreachBankStorageGainForEachRRFRelease(NumberMonumentCreekSubreaches+ &
     NumberFountainCreekSubreaches,MaximumNumberOfRRFReleases), &
     SubreachBankStorageReleasedDuringRecovery(NumberMonumentCreekSubreaches+NumberFountainCreekSubreaches, &
     MaximumNumberOfRRFReleases,MaximumNumberOfBankStorageRecoveryDays))
SubreachBankStorageLossForEachRRFRelease=0.0
SubreachBankStorageGainForEachRRFRelease=0.0
SubreachBankStorageReleasedDuringRecovery=0.0

allocate (NativeFlowDiversionAlongMonumentCreek(NumberMonumentCreekDiversions), &
     NativeFlowDiversionAlongFountainCreek(NumberFountainCreekDiversions), &
     FryArkFlowDiversionAlongFountainCreek(NumberFountainCreekDiversions), &
     ExchangeDiversion1AlongFountainCreek(NumberFountainCreekDiversions), &
     CSRRFDiversion2AlongFountainCreek(NumberFountainCreekDiversions))
NativeFlowDiversionAlongMonumentCreek=0.0
NativeFlowDiversionAlongFountainCreek=0.0
FryArkFlowDiversionAlongFountainCreek=0.0
ExchangeDiversion1AlongFountainCreek=0.0
CSRRFDiversion2AlongFountainCreek=0.0

allocate (SubreachSumNativeFlowDiversionsAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SegmentSumNativeFlowDiversionsAlongMonumentCreek(NumberMonumentCreekSegments))
SubreachSumNativeFlowDiversionsAlongMonumentCreek=0.0
SegmentSumNativeFlowDiversionsAlongMonumentCreek=0.0

allocate (SubreachSumNativeFlowDiversionsAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachSumFryArkDiversionsAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachSumExchangeDiversionsAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachSumReusedDiversionsAlongFountainCreek(NumberFountainCreekSubreaches), &
     SegmentSumNativeFlowDiversionsAlongFountainCreek(NumberFountainCreekSegments), &
     SegmentSumFryArkDiversionsAlongFountainCreek(NumberFountainCreekSegments), &
     SegmentSumExchangeDiversionsAlongFountainCreek(NumberFountainCreekSegments), &
     SegmentSumReusedDiversionsAlongFountainCreek(NumberFountainCreekSegments))
SubreachSumNativeFlowDiversionsAlongFountainCreek=0.0
SubreachSumFryArkDiversionsAlongFountainCreek=0.0
SubreachSumExchangeDiversionsAlongFountainCreek=0.0
SubreachSumReusedDiversionsAlongFountainCreek=0.0
SegmentSumNativeFlowDiversionsAlongFountainCreek=0.0
SegmentSumFryArkDiversionsAlongFountainCreek=0.0
SegmentSumExchangeDiversionsAlongFountainCreek=0.0
SegmentSumReusedDiversionsAlongFountainCreek=0.0

allocate (TransitLossSavingsDiversion(NumberOfDiversions), &
    SegmentTransitLossFromFromFountainCreekDiversion(NumberFountainCreekSegments), &
    SubreachTransitLossFromFountainCreekDiversion(NumberFountainCreekSubreaches))
TransitLossSavingsDiversion=0.0
SegmentTransitLossFromFromFountainCreekDiversion=0.0
SubreachTransitLossFromFountainCreekDiversion=0.0


allocate (UpstreamReleaseInFountainCreekNeededToDeliverFryArkReturnFlow(NumberFountainCreekDiversions), &
     UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow(NumberFountainCreekDiversions), &
     UpstreamReleaseInFountainCreekNeededToDeliverReusedReturnFlow(NumberFountainCreekDiversions))

     
allocate (SubreachBankStorageGainAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachBankStorageLossAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachChannelStorageGainAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachChannelStorageLossAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachDownstreamNativeFlowAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachDownstreamTotalRRFAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachEvaporationLossFountainCreek(NumberFountainCreekSubreaches), &
     SubreachGainOrLossNativeFlowAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachGainOrLossReusableFlowAlongFountainCreek_pct(NumberFountainCreekSubreaches), &
     SubreachGainOrLossAllReusableFlowAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachUpstreamNativeFlowAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachUpstreamTotalRRFAlongFountainCreek(NumberFountainCreekSubreaches), &
     SubreachGainOrLossNativeFlowAlongFountainCreek_pct(NumberFountainCreekSubreaches))     

allocate (SubreachBankStorageGainAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachBankStorageLossAlongMounumentCreek(NumberMonumentCreekSubreaches), &
     SubreachChannelStorageGainAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachChannelStorageLossAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachDownstreamNativeFlowAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachDownstreamTotalRRFAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachEvaporationLossMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachGainOrLossNativeFlowAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachGainOrLossReusableFlowAlongMonumentCreek_pct(NumberMonumentCreekSubreaches), &
     SubreachGainOrLossAllReusableFlowAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachUpstreamNativeFlowAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachUpstreamTotalRRFAlongMonumentCreek(NumberMonumentCreekSubreaches), &
     SubreachGainOrLossNativeFlowAlongMonumentCreek_pct(NumberMonumentCreekSubreaches))    
SubreachBankStorageGainAlongMonumentCreek=0.0
SubreachBankStorageLossAlongMounumentCreek=0.0
SubreachDownstreamNativeFlowAlongMonumentCreek=0.0
SubreachDownstreamTotalRRFAlongMonumentCreek=0.0
SubreachEvaporationLossMonumentCreek=0.0
SubreachGainOrLossNativeFlowAlongMonumentCreek=0.0
SubreachGainOrLossAllReusableFlowAlongMonumentCreek=0.0
SubreachUpstreamNativeFlowAlongMonumentCreek=0.0
SubreachUpstreamTotalRRFAlongMonumentCreek=0.0
SubreachGainOrLossReusableFlowAlongMonumentCreek_pct=0.0
SubreachGainOrLossNativeFlowAlongMonumentCreek_pct=0.0

!#Set InputErrorFlag initially to 0 to denote no IO error.
InputErrorFlag=0

!# Set length of working path name.
InputFilePathnameLength=len(trim(InputFilePathname))

!Open main web input file (open here so output file can be set).
!#Find the length of the web input filename.
InputOrOutputFilename = 'WebIn.fil'
InputFilenameLength=len(trim(InputOrOutputFilename))

!#Concatenate the input directory name and the web input filename to get the web input file pathname.
FilePathname = trim(InputFilePathname(:InputFilePathnameLength)//InputOrOutputFilename(:InputFilenameLength))

!#Open the web input file as an existing file using file unit 61. If the OPEN statement causes an exception (ERR=910), 
!#print 'In MAIN--Options 1-3, Unable to open "Webin.fil" file.' using the format statement given by label 910.
open(unit=WebInputFileUnit,file=FilePathName,status='old',iostat=iostatus)

! File open error , tell the user why the open failed.
if (iostatus > 0) then
     FileWithIOError = FilePathName
      InputErrorFlag = 1
      print *, 'Cannot open file WebIn.fil'
      write (TextOutputFileUnit,1010) 'In MAIN--options 1-3, unable to open "Webin.fil" file.'
      call ReportInputError()
     Go to 9999
end if

!Read date for current run, create name for output file, and open it.
!#Using format statement 1000, read the date on the first line of the web input file as an 8-character variable. 
read(WebInputFileUnit,1000,iostat=iostatus) InputDate

!# Error on first line of WebIn.fil
if (iostatus /= 0) then
    write (TextOutputFileUnit,1010) 'Problem reading first line of  "Webin.fil" file.'
     InputErrorFlag = 1
    Go to 9999
end if


!#Using the date read from the web input file, create the name of the .txt and .csv output files by concatenating the date 
!#to the prefix 'tl' and adding '.txt' and '.csv' file extensions.
call Outname(RunOption,InputErrorFlag)

!#If creating the output filenames caused an error (InputErrorFlag>0), then call subroutine CloseFiles to close open files and stop execution.
if (InputErrorFlag > 0) then
     Go to 9999
end if


!*****************************************************************************
!** option 1 ** compute transit losses for current day
!** option 2 ** recompute transit losses for previous day
!** option 3 ** recompute transit losses for day prior to previous day
!*****************************************************************************

CheckForComputeTransitLosses: if(RunOption > 0 .and. RunOption <= 3) then

     
     !Open program input, data, and backup files (except main web input).
     !#If you're actually running the code and not just updating the accounts file 'Accts.fil', then
     !#open the output files you created pathnames for in subroutine OutName.
    call OpenFiles(InputErrorFlag)
 
    !#If an error occurs in subroutine OpenFiles, then close all files and terminate program execution.
    if (InputErrorFlag > 0) then
           Go to 9999
     end if
      
     !Assign the file unit code for the input file based on the option chosen.
     select case(RunOption)
          case(1)
               FileUnitCode=LastInputFileUnit
          case(2)
               FileUnitCode=BACK1FileUnit
          case(3)
               FileUnitCode=BACK2FileUnit
          case default
               print*, 'Problem with run option entered.'
               go to 9999
     end select
      
     !Read last date (depending on value of RunOption).
     !#Read the last date from the relevant file and go back one line for subsequent reading from the same file.
     read(FileUnitCode,1000) PreviousRunDate
     backspace (FileUnitCode)

     !Check for right date increment.
     !#Check to see if the input date is 1 day greater than the last date for which transit loss computations were last made.
     call Datechk (InputErrorFlag)
     !#If not valid date, close all files and bail on the run.
     if (InputErrorFlag > 0) then
          Go to 9999
     end if

     !Set ReleaseDate and ExchangeDate for program run.
     !#Set release date (ReleaseDate) and exchange date (ExchangeDate).
     call EndDate
     
     !Call  input subroutine to read and write backup files and read all input files from web page (Webin.fil, and others).
     !#Read discharge at the gaging stations; native, transmountain, and other reusable return flows;
     !#native and transmountain diversions; and accounts data.
     call DataIn(RunOption,InputErrorFlag)
     
     !#If an error occurs in subroutine DataIn, close all files and terminate program execution.
     if (InputErrorFlag > 0) then
          ! FileWithIOError = FilePathName
          Go to 9999
     end if

     !Open and load bank storage file.
     !#Read arrays (look-up tables) for bank storage loss in subreaches for Monument and Fountain Creeks to be used for transit 
     !#loss calculations.
     call ReadBankStorageLossLookupTable (InputErrorFlag)

     !#If an error occurs in subroutine ReadBankStorageLossLookupTable, close all files and terminate program execution.
     if (InputErrorFlag > 0) then
          ! FileWithIOError = FilePathName
          Go to 9999
     end if

     !Read recovery file into array for random access during transit loss comp.
     !#Read the bank loss recovery file a single time and then save the data in an array for random access during transit loss 
     !#computations so that you don't have to read and rewind the bank loss recovery file after each iteration.
     call ReadBankStorageLossRecoveryTable (InputErrorFlag)

     !#If an error occurs in subroutine ReadBankStorageLossRecoveryTable, close all files and terminate program execution.
     if (InputErrorFlag > 0) then
          ! FileWithIOError = FilePathName
          Go to 9999
     end if
     
     !Call subroutines to compute transit losses.
     !#Estimate transit-losses for reusable return flows along Monument Creek.
     call EstimateMonumentCreekRRFTransitLosses (NetRRFQInMonumentDischargedToFountain,NetNativeQInMonumentDischargedToFountain, &
          InputErrorFlag)

     !#If an error occurs in subroutine EstimateMonumentCreekRRFTransitLosses, close all files and 
      !#terminate program execution. No way to set error on calculations.
     !#Possible error: InputErrorFlag is never assigned a value in subroutine 
      !#EstimateMonumentCreekRRFTransitLosses, so if runtime errors
     !#are occurring in subroutine EstimateMonumentCreekRRFTransitLosses, the user has no way of knowing this.
    
     if (InputErrorFlag > 0) then
          ! FileWithIOError = FilePathName
          Go to 9999
     end if

     !New subroutine to initialize common blocks used in Fountain Creek subroutine. Because of potential for two Fountain Creek transit loss calculations,
     !problem develops as to where to initialize. Not logical to do it elsewhere.
     !#Initialize a bunch of stuff for the fountain creek calculations.
     !call fountn_init
     UpstreamReleaseInFountainCreekNeededToDeliverFryArkReturnFlow=0.0
     UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow=0.0
     UpstreamReleaseInFountainCreekNeededToDeliverReusedReturnFlow=0.0

     SubreachBankStorageGainAlongFountainCreek=0.0
     SubreachBankStorageLossAlongFountainCreek=0.0
     SubreachDownstreamNativeFlowAlongFountainCreek=0.0
     SubreachDownstreamTotalRRFAlongFountainCreek=0.0
     SubreachEvaporationLossFountainCreek=0.0
     SubreachGainOrLossNativeFlowAlongFountainCreek=0.0
     SubreachGainOrLossAllReusableFlowAlongFountainCreek=0.0
     SubreachUpstreamNativeFlowAlongFountainCreek=0.0
     SubreachUpstreamTotalRRFAlongFountainCreek=0.0
     SubreachGainOrLossReusableFlowAlongFountainCreek_pct=0.0
     SubreachGainOrLossNativeFlowAlongFountainCreek_pct=0.0
     
     CSURRFDeliveryWithTransitLossSavings = 0.0
     CSURRFDeliveryWithoutTransitLossSavings = 0.0

     !#If FlagToComputeNetGainWater is TRUE, then call subroutine EstimateFountainCreekRRFTransitLossesWithExchange, 
      !# which is a duplicate of subroutine EstimateFountainCreekRRFTransitLosses, 
     !#but called only when any of 8 SE exchange diversions are specified on input, producing a different 
     !#calculation of transit loss at the mouth of Fountain Creek.
     if (FlagToComputeNetGainWater)  call EstimateFountainCreekRRFTransitLossesWithExchange &
     (NetRRFQInMonumentDischargedToFountain,NetNativeQInMonumentDischargedToFountain,InputErrorFlag)

     !#If an error occurs in subroutine EstimateFountainCreekRRFTransitLossesWithExchange, close all files 
      !#and terminate program execution.  No way to set error on calculations
    if (InputErrorFlag > 0) then
           ! FileWithIOError = FilePathName
           Go to 9999
     end if

     !# Compute for all the rest of Fountain Creek water.
     call EstimateFountainCreekRRFTransitLosses (NetRRFQInMonumentDischargedToFountain,NetNativeQInMonumentDischargedToFountain, &
     InputErrorFlag)

     !#if an error occurs in subroutine EstimateFountainCreekRRFTransitLosses, close all files and terminate program execution.  No way to set error flag on calculations
    if (InputErrorFlag > 0) then
           ! FileWithIOError = FilePathName
           Go to 9999
     end if

     !Write output report.
     !#Write the output report for each day of program run, except the last page, which is written 
     !#in subroutine ReadAndWriteAccumulatedRRFs.
     call WriteTransitLossOutput (InputErrorFlag)
      
      if (InputErrorFlag > 0) then
           ! FileWithIOError = FilePathName
           Go to 9999
      end if

     !#Print 'Complete for Release: ' in the DOS window.
     print *, 'Model run complete for Release Date: ',InputDate

     !# All done.  Close all open files and terminate execution.
     

!****************************************************************************************
!***   option 4   *** change or update account table file "acct.fil" for next program run
!****************************************************************************************

!Open the file that contains the new accounts data input on web form
!and written to "Accts_in.fil."

!#The user chose option 4, so just update the accounts file acct.fil.
 else if (RunOption ==4) then
    !#Create the pathname for the Accts_in.fil and open the file.
     InputOrOutputFilename = 'Accts_in.fil'
     ! InputFilenameLength = index(InputOrOutputFilename,' ') -1
     InputFilenameLength = len(trim(InputOrOutputFilename))
     InputOrOutputFilename = InputFilePathname(:InputFilePathnameLength)//InputOrOutputFilename(:InputFilenameLength)
     !#Open file Accts_in.fil, .
     open (unit=INACCTSFileUnit,file=InputOrOutputFilename,status='old',IOStat=iostatus)
     
     !  Error, then print message and bail
     if (iostatus > 0) then
           FileWithIOError = InputOrOutputFilename
           write (TextOutputFileUnit,1010) 'In MAIN--option 4, unable to open "Accts_in.fil" file.'
           call ReportInputError()
           InputErrorFlag = 1
           Go to 9999
     end if
           
     !Open "acct.fil" which will be updated for next program run. Subroutine
     !"open" not called, so file needs to be opened.

     !#Create the pathname for the acct.fil and open the file.
     InputOrOutputFilename = 'acct.fil'
  
     InputFilenameLength = len(trim(InputOrOutputFilename))
     InputOrOutputFilename = InputFilePathname(:InputFilePathnameLength)//InputOrOutputFilename(:InputFilenameLength)
     !#Open file acct.fil, 
     open (unit=ACCTSFileUnit,file=InputOrOutputFilename,status='old',IOStat=iostatus)
     
     !  Error, then print message and bail
     if (iostatus > 0) then
           FileWithIOError = InputOrOutputFilename
           write (TextOutputFileUnit,1010) 'In MAIN--option 4, unable to open "acct.fil" file.'
           call ReportInputError()
           InputErrorFlag = 1
           Go to 9999
     end if 

!#Update the accounts file.
     call UpdateAccountsFile (INACCTSFileUnit,ACCTSFileUnit,InputErrorFlag)
     
     !#If no error occurred in subroutine UpdateAccountsFile, tell the user that the accounts file was updated successfully.
     if (InputErrorFlag == 0) then
          write (TextOutputFileUnit,1010)'*** ACCOUNTS FILE UPDATED SUCCESSFULLY.'
          write (TextOutputFileUnit,1010)'*** PLEASE VERIFY UPDATES ON WEB PAGE.'
          print *,'*** ACCOUNTS FILE UPDATED SUCCESSFULLY.'
          print *,'*** PLEASE VERIFY UPDATES ON WEB PAGE.'
          !#Close the accounts file
         close (INACCTSFileUnit)
     else
          write (TextOutputFileUnit,1010)'*** PROBLEM WITH ACCOUNTS FILE!'
          write (TextOutputFileUnit,1010)'*** PLEASE REVIEW ON WEB PAGE.'
          print *,'*** PROBLEM WITH ACCOUNTS FILE!'
          print *,'*** PLEASE REVIEW ON WEB PAGE.'
          close (INACCTSFileUnit)
          Go to 9999
     end if
          
 else
     !#None of the valid options 1-4 was chosen, so write 'In MAIN--invalid program option.' to let the user know.
    write (TextOutputFileUnit,1010) 'In MAIN--invalid program option. Must include option 1 - 4.'

end if CheckForComputeTransitLosses

1000 format (a8)
1010 format (a100)

!# All Done
!#  Good or model fail we close files and get out
9999 Call CloseFiles
if (InputErrorFlag > 0) then
     !#Print 'error to console.
     print *, 'ERROR!!!  Model run incomplete.  Review error and run again.'
end if

end program Main

!*****************************************************************************
subroutine Outname(RunOption,InputErrorFlag)
!*****************************************************************************
!Subroutine creates a date-based name for output report file and opens it.

!Definition of local variables:
!-----------------------------------------------------------------------------
!TextOutputFilename = Name of file for output report.
!*****************************************************************************
use CommonVariables, only: InputFilePathname,OutputFilePathname,InputOrOutputFilename,InputDate, &
     CSVOutputFileUnit,FileWithIOError,TextOutputFileUnit,iostatus
implicit none
      
integer :: InputErrorFlag
integer, intent(in) :: RunOption
integer :: InputFilePathnameLength

character :: TextOutputFilename*15

!#Start with a blank output filename, then append to the filename.
data TextOutputFilename /''/

InputErrorFlag=0

!Added different output filename for option 4 so an existing standard daily
!output file doesn't get overwritten, losing output results, requiring rerun.
!Append .txt to filename (tlyyyymmdd.txt)

!#If the user didn't choose option 4, then create a text output filename with prefix 'tl', concatenated with year, month, day 
!#from InputDate read from web input file, with the '.txt' extension.
!#If the user chose option 4 to simply update the accounts file and not execute an actual transit-loss run, 
!#then write output to file 'Acct_upd.out'. Otherwise, build the output filename base on the input date.

SelectTextTextOutputFilenameName: select case (RunOption)
     case (1:3)
          TextOutputFilename(1:2)='tl'
          TextOutputFilename(3:6)=InputDate(5:8)
          TextOutputFilename(7:8)=InputDate(1:2)
          TextOutputFilename(9:10)=InputDate(3:4)
          TextOutputFilename(11:14)='.txt'
     case (4)
         TextOutputFilename='Acct_upd.out'
     case default
          InputErrorFlag = 1
          write(TextOutputFileUnit,1000) 'In Subroutine: Outname. Error with Run Option '
          return
end select SelectTextTextOutputFilenameName

!#Determine the length of the output filename. 
!#Used nested intrinsic functions len(trim) instead of index to get the filename length.
InputFilePathnameLength=len(trim(OutputFilePathname))
InputOrOutputFilename=OutputFilePathname(:InputFilePathnameLength)//TextOutputFilename

!#Delete the old output file by opening it, then deleting on the close. Otherwise, you'd have to use system commands,
!#which depend on the operating system.
!#If an error occurs when the file is opened, write 'In subroutine Outname--unable to open program output file.' to the .txt output file.
open(unit=TextOutputFileUnit,file=InputOrOutputFilename,status='unknown',iostat=iostatus)
!# if file open error, write message and bail
if (iostatus /= 0) then
     FileWithIOError=InputOrOutputFilename
      call ReportInputError()
      write(TextOutputFileUnit,1000) 'In subroutine Outname--Unable to old file and delete program output file.'
     InputErrorFlag=1
     return
end if 

close(unit=TextOutputFileUnit,status='delete')
open(unit=TextOutputFileUnit,file=InputOrOutputFilename,status='new',iostat=iostatus)
!# if file open error, write message and bail
if (iostatus /= 0) then
     FileWithIOError=InputOrOutputFilename
      call ReportInputError()
      write(TextOutputFileUnit,1000) 'In subroutine Outname--Unable to open program output file.'
     InputErrorFlag=1
     return
end if 

!Open output file for csv output if option 1-3.
!Just append .csv to tlyyyymmdd filename, then open.
SelectCSVTextOutputFilenameName: select case (RunOption)
     !#Append the .csv extension to the filename.
     case (1:3)
          TextOutputFilename(11:14)='.csv'
          !#Use nested intrinsic functions len(trim) instead of index to get the filename length.
          InputFilePathnameLength = len(trim(OutputFilePathname))
          !#Append the filename to the directory name to get the full pathname of the output file.
          InputOrOutputFilename = OutputFilePathname(:InputFilePathnameLength)//TextOutputFilename
          !#Delete the old output file by opening it, then deleting on the close. Otherwise, you'd have to use system commands,
          !#which depend on the operating system.
          !#If an error occurs when the file is opened, write 'In subroutine Outname--Unable to open program output file.' to the .csv output file.
          open(unit=CSVOutputFileUnit,file=InputOrOutputFilename,status='unknown',iostat=iostatus)
          !# if file open error, write message and bail
            if (iostatus /= 0) then
                 FileWithIOError=InputOrOutputFilename
                 call ReportInputError()
                 write(TextOutputFileUnit,1000) 'In subroutine Outname--Unable to open old csv program output file.'
                 InputErrorFlag=1
                 return
             end if 
          
          close(unit=CSVOutputFileUnit,status='delete')
          open(unit=CSVOutputFileUnit,file=InputOrOutputFilename,status='new',iostat=iostatus)
          !# if file open error, write message and bail
            if (iostatus /= 0) then
                 FileWithIOError=InputOrOutputFilename
                 call ReportInputError()
                 write(TextOutputFileUnit,1000) 'In subroutine Outname--Unable to open new csv program output file.'
                 InputErrorFlag=1
                 return
             end if 
         
end Select SelectCSVTextOutputFilenameName

return

1000 format(a80)
end

!*****************************************************************************
subroutine OpenFiles(InputErrorFlag)
!*****************************************************************************
!Subroutine opens most of the I/O files for the program.

!See "files.cmn" for a description of all files used in program, including the
!external file name, unit name/number in program,and how the file is used in
!the program.

!#unit    filename            variable
!#----  --------            ---------
!# 59  Mon_low.bsfile.dat     BankStorageLookupTableForLowMonumentCreekFlowsFileUnit  
!# 60  both.bsfile.dat        BankStorageLookupTableFileUnit   
!# 61  WebIn.fil              WebInputFileUnit              
!# 62  TextOutputFileUnit
!# 63  last.WebIn.fil         LastInputFileUnit        
!# 64  back1.fil              BACK1FileUnit        
!# 65  back2.fil              BACK2FileUnit         
!# 66  acct.fil               ACCTSFileUnit         
!# 67  both_recov.fil         BankStorageRecoveryFileUnit  
!# 68  reusable_sum.fil       ACCUMFileUnit                
!# 69  scratch1               SCRATCH1FileUnit          
!# 70  scratch2               SCRATCH2FileUnit             
!# 71  CSVOutputFileUnit

!*****************************************************************************
use CommonVariables, &
     only : InputFilePathname,ACCTSFileUnit,ACCUMFileUnit,BACK1FileUnit,BACK2FileUnit,BankStorageRecoveryFileUnit, &
     TextOutputFileUnit, FileWithIOError,LastInputFileUnit,SCRATCH1FileUnit,SCRATCH2FileUnit,iostatus
implicit none

integer, intent(out) :: InputErrorFlag
integer :: InputFilePathnameLength,InputFilenameLength,InputFileNumber,ScratchFileUnitNumber, &
     InputFileUnitNumbers(6),OpenErrorLabels(6),ScratchFileUnitNumbers(2),ErrorLabel
character(20) :: InputFilename(6),ScratchFilename(2)

!Make OpenFilePathName allocatable so that you can later trim it down with no trailing blanks.

character :: OpenFilePathName*250

data InputFilename /'last.WebIn.fil','back1.fil','back2.fil','acct.fil','both_recov.fil','reusable_sum.fil'/
data ScratchFilename /'scratch1','scratch2'/

     

InputFileUnitNumbers(1:6)=(/LastInputFileUnit,BACK1FileUnit,BACK2FileUnit,ACCTSFileUnit,BankStorageRecoveryFileUnit, &
     ACCUMFileUnit/)
ScratchFileUnitNumbers(1:2)=(/SCRATCH1FileUnit,SCRATCH2FileUnit/)

!#InputErrorFlag defaults to 0.
InputErrorFlag=0

!#Use len(trim()) to get the exact length of the directory name so you can use it to create absolute pathnames for all input files.

InputFilePathnameLength=len(trim(InputFilePathname))

LoopThroughAllInputFilesAndOpen: do InputFileNumber=1,6
 
     !#If unable to open the file, write an error to the .txt output file.
     InputFilenameLength = len(trim(InputFilename(InputFileNumber)))
     OpenFilePathName = trim(InputFilePathname(:InputFilePathnameLength))//InputFilename(InputFileNumber)(:InputFilenameLength)
     open(unit=InputFileUnitNumbers(InputFileNumber),file=OpenFilePathName,status='old',iostat=iostatus)
     
      if (iostatus /= 0) then
           select case(InputFileNumber)
                 case(1)
                       write (TextOutputFileUnit,1000) 'In subroutine OPEN--Unable to open "last.WebIn.fil" file.'
                 case(2)
                       write (TextOutputFileUnit,1000) 'In subroutine OPEN--Unable to open "back1.fil" file.'
                 case(3)
                       write (TextOutputFileUnit,1000) 'In subroutine OPEN--Unable to open "back2.fil" file.'
                 case(4)
                       write (TextOutputFileUnit,1000) 'In subroutine OPEN--Unable to open "acct.fil" file.'
                 case(5)
                       write (TextOutputFileUnit,1000) 'In subroutine OPEN--Unable to open "both_recov.fil" file.'
                 case(6)
                       write (TextOutputFileUnit,1000) 'In subroutine OPEN--Unable to open "reusable_sum.fil" file.'
          end select
            
           FileWithIOError=OpenFilePathName
            call ReportInputError()
          InputErrorFlag=1
            return
     end if
      
end do LoopThroughAllInputFilesAndOpen

LoopThroughBothScratchFilesAndOpen: do ScratchFileUnitNumber=1,2
     !#Use len(trim()) to get the exact length of the filename for each scratch file, then append the filename to the directory name to get the full pathname and open the file.
     !#If unable to open the file, write an error to the .txt output file.
     InputFilenameLength = len(trim(ScratchFilename(ScratchFileUnitNumber)))
     OpenFilePathName = trim(InputFilePathname(:InputFilePathnameLength))//ScratchFilename(ScratchFileUnitNumber) &
          (:InputFilenameLength)
     open(unit=ScratchFileUnitNumbers(ScratchFileUnitNumber),status='scratch',iostat=iostatus)
    
      if (iostatus /= 0) then
           select case(InputFileNumber)
                 case(1)
                       write (TextOutputFileUnit,1000) 'In subroutine OPEN--Unable to open "scratch 1" file.'
                 case(2)
                       write (TextOutputFileUnit,1000) 'In subroutine OPEN--Unable to open "scratch 2" file.'
          end select
           FileWithIOError = OpenFilePathName
            call ReportInputError()
          InputErrorFlag = 1
            return
     end if
      
end do LoopThroughBothScratchFilesAndOpen

1000 format (1x,a80,/)

return
end

!*****************************************************************************
subroutine Datechk (InputErrorFlag)
!*****************************************************************************
!Subroutine checks on date increment.

!Definition of local variables:
!-----------------------------------------------------------------------------
!NumberOfDaysInMonth = an array of the number of days in a month, January--December.

!FlagStartOfNewMonth = a logical variable to determine when a new month starts.

!CheckForSameOrConsecutivMonthOfExchangeDate = a integer value for difference between current and last month.
!*****************************************************************************
use CommonVariables, &
     only : InputDate,DayOfCurrentReleaseDate,DayOfLastReleaseDate,MonthOfCurrentReleaseDate, &
     MonthOfLastReleaseDate,PreviousRunDate,TextOutputFileUnit,YearOfCurrentReleaseDate,YearOfLastReleaseDate, iostatus
implicit none

integer, intent (out) :: InputErrorFlag
integer ::  NumberOfDaysInMonth(12),CheckForSameOrConsecutivMonthOfExchangeDate

!character :: InputDate*8,PreviousRunDate*8
logical :: FlagStartOfNewMonth

!#Number of days in each month.
data NumberOfDaysInMonth /31,28,31,30,31,30,31,31,30,31,30,31/

InputErrorFlag=0
!#Read the month, day, and year for the current date and the date for the previous day 
!#using the 8-digit integer dates InputDate and PreviousRunDate.
read (InputDate,'(i2,i2,i4)',iostat=iostatus) MonthOfCurrentReleaseDate,DayOfCurrentReleaseDate,YearOfCurrentReleaseDate
read (PreviousRunDate,'(i2,i2,i4)',iostat=iostatus) MonthOfLastReleaseDate,DayOfLastReleaseDate,YearOfLastReleaseDate

if (iostatus /= 0) then
     write (TextOutputFileUnit,*) "Error reading Release Date."
     print *,  "Error reading Release Date."
     InputErrorFlag = 1
     return
end if

!#Change the number of days in February to 29 for leap years.
if (MonthOfCurrentReleaseDate == 2 .and. mod(YearOfCurrentReleaseDate,4) == 0) NumberOfDaysInMonth(MonthOfCurrentReleaseDate) = 29
if (MonthOfLastReleaseDate == 2 .and. mod(YearOfLastReleaseDate,4) == 0) NumberOfDaysInMonth(MonthOfLastReleaseDate) = 29
FlagStartOfNewMonth=.FALSE.

!Compare input date with last date used in computations.
!Input date should be incremented by 1 from last date.

!Check on year increment.

!#If the current date occurs in the same year as the year of the last date, skip year increment calculations.
CheckYearIncrement: if (YearOfCurrentReleaseDate == YearOfLastReleaseDate) then
     CheckMonthIncrement: if (MonthOfCurrentReleaseDate == MonthOfLastReleaseDate) then
          goto 20
          
     !#If the current and last dates fall in consecutive months, turn on the flag FlagStartOfNewMonth on and proceed to check the daily increment.
     else if (MonthOfCurrentReleaseDate /= MonthOfLastReleaseDate) then
          CheckForSameOrConsecutivMonthOfExchangeDate=MonthOfCurrentReleaseDate-MonthOfLastReleaseDate
          CheckForConsecutivMonthOfExchangeDateths: if (CheckForSameOrConsecutivMonthOfExchangeDate == 1) then
               FlagStartOfNewMonth=.TRUE.
               goto 20
                
          else
               !#Otherwise, the current date and the last date don't occur during the same or consecutive months and are not valid dates, 
               !#so turn on the InputErrorFlag flag to terminate the run after control transfers back to the main program.
               write (TextOutputFileUnit,1000) MonthOfCurrentReleaseDate,DayOfCurrentReleaseDate,YearOfCurrentReleaseDate, &
                    MonthOfLastReleaseDate,DayOfLastReleaseDate,YearOfLastReleaseDate
               InputErrorFlag = 1
               return
          end if CheckForConsecutivMonthOfExchangeDateths
     end if CheckMonthIncrement
!#If the current date occurs 1 year after the year associated with the last date, then check to see if
!#the current date falls in January and the last date falls in December of the preceding year.
else if (YearOfCurrentReleaseDate == YearOfLastReleaseDate+1) then
     CheckForSameOrConsecutivMonthOfExchangeDate=MonthOfCurrentReleaseDate-MonthOfLastReleaseDate
     !#If the dates don't fall in consecutive months spanning different years, turn on the InputErrorFlag flag to terminate the run after control transfers back to the main program.
     !#If the dates fall in consecutive months spanning different years, turn the flag FlagStartOfNewMonth on and proceed to check the daily increment.
     if (CheckForSameOrConsecutivMonthOfExchangeDate /= -11) then
          write (TextOutputFileUnit,1000) MonthOfCurrentReleaseDate,DayOfCurrentReleaseDate,YearOfCurrentReleaseDate, &
               MonthOfLastReleaseDate,DayOfLastReleaseDate,YearOfLastReleaseDate
          InputErrorFlag = 1
          return
     else
          FlagStartOfNewMonth=.TRUE.
          goto 20
     end if
     
else
     !#Otherwise, the current date and the last date don't occur during the same year or in consecutive months of sequential years and are not valid dates, 
     !#so turn on the InputErrorFlag flag to terminate the run after control transfers back to the main program.
     write (TextOutputFileUnit,1000) MonthOfCurrentReleaseDate,DayOfCurrentReleaseDate,YearOfCurrentReleaseDate, &
          MonthOfLastReleaseDate,DayOfLastReleaseDate,YearOfLastReleaseDate
     InputErrorFlag = 1
     return
end if CheckYearIncrement

!Check on day increment.
!#The dates fall in the same month and year, so test for 1 day difference between the dates to determine if the dates are valid.
20 CheckDayIncrement: if (.not. FlagStartOfNewMonth) then
     !INCORRECT STATEMENT "IF (DayOfCurrentReleaseDate .EQ. DayOfLastReleaseDate+1 .AND. DayOfCurrentReleaseDate .LE .NumberOfDaysInMonth(MonthOfLastReleaseDate)) THEN"
     !because of space between "LE" and ".". Fails to compile using most compilers except cygwin.
     CheckFor1DayApartSamMonthOfExchangeDateth: if (DayOfCurrentReleaseDate == DayOfLastReleaseDate+1 .and. &
          DayOfCurrentReleaseDate <= NumberOfDaysInMonth(MonthOfLastReleaseDate)) then
          !#The dates are 1 day apart and valid, so return to the main program without error.
          return
     else
          !#The dates are not 1 day apart and are invalid, so turn on the InputErrorFlag flag to terminate the run after control transfers back to the main program.
          write (TextOutputFileUnit,1000) MonthOfCurrentReleaseDate,DayOfCurrentReleaseDate,YearOfCurrentReleaseDate, &
               MonthOfLastReleaseDate,DayOfLastReleaseDate,YearOfLastReleaseDate
          InputErrorFlag = 1
          return
     end if CheckFor1DayApartSamMonthOfExchangeDateth
else if (FlagStartOfNewMonth) then
     !#The dates either fall in different but consecutive months in the same year or in different but consecutive years, so test to see if 
     !#1 day apart and fall in the same month.
     CheckFor1DayApartDifferentMonths: if (DayOfCurrentReleaseDate == 1 .and. &
          DayOfLastReleaseDate == NumberOfDaysInMonth(MonthOfLastReleaseDate)) then
          return
     else
          !#If the current day doesn't fall on the 1st of the month and the last day doesn't fall on the final day of the previous month, the dates
          !#are not valid so turn on the InputErrorFlag flag to terminate the run after control transfers back to the main program.
          write (TextOutputFileUnit,1000) MonthOfCurrentReleaseDate,DayOfCurrentReleaseDate,YearOfCurrentReleaseDate, &
               MonthOfLastReleaseDate,DayOfLastReleaseDate,YearOfLastReleaseDate
          InputErrorFlag = 1
          return
     end if CheckFor1DayApartDifferentMonths
end if CheckDayIncrement



!#Turn the error flag on so that when control transfers back to the main calling program, the run will terminate.


999 return

1000 format (/,6x,'*** INVALID DATE SEQUENCE ***',//, 6x,'INPUT DATE:  ',2(i2.2,'-'),i4.2,', IS NOT 1 DAY ', &
        'GREATER THAN',/,7x,'LAST DATE:  ',2(i2.2,'-'),i4.2,', FOR WHICH TRANSIT LOSS COMPUTATIONS',/, 33x,'WERE LAST COMPLETED.')
end

!*****************************************************************************
subroutine EndDate
!*****************************************************************************
!Subroutine to set release date (ReleaseDate) and exchange date (ExchangeDate).

!Definition of local variables:
!-----------------------------------------------------------------------------
!NumberOfDaysInMonth = an array of the number of days in a month, january--december.

!DayOfExchangeDate,MonthOfExchangeDate,YearOfExchangeDate = day, month, and year for setting the exchange date (ExchangeDate).
!*****************************************************************************
use CommonVariables, &
     only : DayOfCurrentReleaseDate,MonthOfCurrentReleaseDate,YearOfCurrentReleaseDate,ExchangeDate,ReleaseDate, iostatus
implicit none

integer :: NumberOfDaysInMonth(12),DayOfExchangeDate,MonthOfExchangeDate,YearOfExchangeDate

!#Define number of days in each month.
data NumberOfDaysInMonth /31,28,31,30,31,30,31,31,30,31,30,31/

!#Set release data and exchange date as the beginning of time?
!Can't alter module values with a data statement, so use assignment instead.
ReleaseDate='00-00-0000'
ExchangeDate='00-00-0000'

!#Print the release month, day, and year to the output file.
write (ReleaseDate,1000) MonthOfCurrentReleaseDate,DayOfCurrentReleaseDate,YearOfCurrentReleaseDate

!#The exchange day, month, and year is initially the same as the release date.
DayOfExchangeDate=DayOfCurrentReleaseDate
MonthOfExchangeDate=MonthOfCurrentReleaseDate
YearOfExchangeDate=YearOfCurrentReleaseDate

!#If the exchange date is in February of a leap year, change the number of days in February from 28 to 29.
if (mod(YearOfExchangeDate,4) == 0 .and. MonthOfExchangeDate == 2) NumberOfDaysInMonth(MonthOfExchangeDate)=29
!#If the exchange date falls on the last day of December, set it to the first day of the next year and then print to the output file.
CheckExchangDayOfExchangeDate: if (MonthOfExchangeDate == 12 .and.  &
     DayOfExchangeDate == NumberOfDaysInMonth(MonthOfExchangeDate)) then
     DayOfExchangeDate=1
     MonthOfExchangeDate=1
     YearOfExchangeDate=YearOfExchangeDate+1
     goto 10
!#Otherwise, if the exchange date falls on the last day of any month, set it equal to the first day of the following month and then print to the output file.
else
     if (DayOfExchangeDate == NumberOfDaysInMonth(MonthOfExchangeDate)) then
          DayOfExchangeDate=1
          MonthOfExchangeDate=MonthOfExchangeDate+1
          goto 10
     end if
end if CheckExchangDayOfExchangeDate

!#Set the exchange date to 1 day after the release date.
DayOfExchangeDate=DayOfExchangeDate+1

10 write (ExchangeDate,1000) MonthOfExchangeDate,DayOfExchangeDate,YearOfExchangeDate

return
1000 format (i2.2,'-',i2.2,'-',i4.2)
end

!*****************************************************************************
subroutine DataIn(RunOption,InputErrorFlag)
!*****************************************************************************
!Subroutine reads input for options 1, 2, or 3. Primarily a series of
!call statements for other subroutines.

!Definition of local variables:
!-----------------------------------------------------------------------------
!ExchangableWater = the amount of Colorado Springs transmountain return flow water that is available for diversion/sale to 
!          native-flow diverters. 

!FryArkWater = the amount of Colorado Springs Fry-Ark transmountain return flow water that is available for diversion/sale to
!          native-flow diverters.

!DummyDate = variable for dummy/temporary date value.

!RecoveryFileHeader = character string at the beginning of "both_recov.fil" used for identification purposes.

!NumberOfNativeDiversionDitchesOnFountainCreek = number of native diversions along Fountain Creek.
!*****************************************************************************
use CommonVariables, &
     only : ACCTSFileUnit,ACCUMFileUnit,BACK1FileUnit,BACK2FileUnit,BankStorageRecoveryFileUnit,InputDate, &
     LastInputFileUnit,PreviousRunDate,SCRATCH1FileUnit,TextOutputFileUnit,AmountOfCSFryArkWaterAvailable, &
     AmountOfCSFryArkWaterUsed,AmountOfCSExchangeWaterAvailable,AmountOfCSExchangeWaterUsed,AmountOfCSWaterAvailable, &
     AmountOfCSWaterUsed, iostatus
implicit none

integer, intent(out) :: InputErrorFlag
integer, intent(in) :: RunOption
real :: FryArkWater,ExchangableWater
      
integer :: NumberOfNativeDiversionDitchesOnFountainCreek,CurrentNativeDiversionDitch
      
character :: DummyDate*8,RecoveryFileHeader*80

!Read names and data values for input stations, return flows, and diversions.
!Data also is used to set dimension size for number of input return
!flows, stations, diversions, and other program variables (arrays).

!Input discharge at the gaging stations.

InputErrorFlag=0
call ReadGagingStationDataInput (InputErrorFlag)
if (InputErrorFlag > 0) return


!Input native, transmountain, and other reusable return flows.
call ReadReturnFlowInput (FryArkWater,ExchangableWater,InputErrorFlag)
if (InputErrorFlag > 0) return

!Input native diversions and transmountain diversion values.
call ReadDiversionDataInput (NumberOfNativeDiversionDitchesOnFountainCreek,InputErrorFlag)
if (InputErrorFlag > 0) return

!Input accounts data (from Accts_in.fil--not part of daily input data).
!Relocated here from before "call ReadDiversionDataInput", G. Kuhn, 08/15/09.
LoopThroughAndReadNativeDiversions: do CurrentNativeDiversionDitch=1,NumberOfNativeDiversionDitchesOnFountainCreek
     read (ACCTSFileUnit,1000,iostat=iostatus) &
          AmountOfCSFryArkWaterAvailable(CurrentNativeDiversionDitch), &
          AmountOfCSFryArkWaterUsed(CurrentNativeDiversionDitch), &
          AmountOfCSExchangeWaterAvailable(CurrentNativeDiversionDitch), &
          AmountOfCSExchangeWaterUsed(CurrentNativeDiversionDitch), &
          AmountOfCSWaterAvailable(CurrentNativeDiversionDitch), &
          AmountOfCSWaterUsed(CurrentNativeDiversionDitch)
         
      ! read error
      if (iostatus /= 0 ) then
            write (TextOutputFileUnit,1030) 'In subroutine DataIn--Error reading Accounts data.'
            InputErrorFlag = 1
            return
      end if
      
end do LoopThroughAndReadNativeDiversions

!#Rewind to the beginning of the file for the next run.
rewind (ACCTSFileUnit)

!Check if transmountain diversions exceed release amounts and account balances.
call CheckForDiversionsExceedingReleasesAndAccountBalance (FryArkWater,ExchangableWater)

!Finish setting dimensions needed for transit loss calculations.
call SetArrayDimensions

!When all done with diversion input and setting dimensions, sum native and transmountain diversions in each segment and in each subreach.
call SumDiversions

!After all data input is complete, re-write backup files and new data.
SelectFileForWritingDataForNextDailyRun: select case (RunOption)

     !** option 1 ** compute transit losses for current day
     !If it's an option 1 run, write BACK1FileUnit to BACK2FileUnit.
     case(1) 
          read (BACK1FileUnit,1010,iostat=iostatus) DummyDate
          if (iostatus /= 0 ) then
               write (TextOutputFileUnit,1030) 'In subroutine DataIn--Read error, Dummy date in back1.'
               InputErrorFlag = 1
               return
          end if 
          backspace (BACK1FileUnit)
          call ReadAndWriteBackupFiles (DummyDate,BACK1FileUnit,BACK1FileUnit,BACK1FileUnit,BACK1FileUnit, &
               BACK2FileUnit, BACK2FileUnit,BACK2FileUnit,BACK2FileUnit,InputErrorFlag)
          
     !** option 2 ** recompute transit losses for previous day
     !If it's a option 2 run, write BACK1FileUnit file to "last.WebIn.fil," "acct.fil,"
     !"reusable_sum.fil," and to "both_recov.fil." BACK2FileUnit is unchanged for now.
     case(2)
          call ReadAndWriteBackupFiles (PreviousRunDate,BACK1FileUnit,BACK1FileUnit,BACK1FileUnit,BACK1FileUnit, &
               LastInputFileUnit,ACCUMFileUnit,ACCTSFileUnit,BankStorageRecoveryFileUnit,InputErrorFlag)
          
     !** option 3 ** recompute transit losses for day prior to previous day
     !If it's a option 3 run, write BACK2FileUnit to "last.WebIn.fil,"  "acct.fil,"
     !"reusable_sum.fil," and to "both_recov.fil." BACK1FileUnit is unchanged for now.
     case(3) 
          call ReadAndWriteBackupFiles (PreviousRunDate,BACK2FileUnit,BACK2FileUnit,BACK2FileUnit,BACK2FileUnit, &
               LastInputFileUnit, ACCUMFileUnit,ACCTSFileUnit,BankStorageRecoveryFileUnit,InputErrorFlag)
end select SelectFileForWritingDataForNextDailyRun

if (InputErrorFlag > 0) return

!Write new input data read from web (WebIn.fil) to 'last.WebIn.fil"
rewind (SCRATCH1FileUnit)
rewind (LastInputFileUnit)
call ReadWebinFileData (SCRATCH1FileUnit,LastInputFileUnit,InputDate,InputErrorFlag)
rewind (SCRATCH1FileUnit)
rewind (LastInputFileUnit)
if (InputErrorFlag > 0) return

!Lastly, write "last.WebIn.fil," "acct.fil," "reusable_sum.fil," and "both_recov.fil" to BACK1FileUnit.
call ReadAndWriteBackupFiles (PreviousRunDate,LastInputFileUnit,ACCUMFileUnit,ACCTSFileUnit,BankStorageRecoveryFileUnit, &
     BACK1FileUnit,BACK1FileUnit,BACK1FileUnit,BACK1FileUnit,InputErrorFlag)

if (InputErrorFlag > 0) return

!Read heading line in "both_recov.fil" so reading of actual data can proceed.
read (BankStorageRecoveryFileUnit,1020,iostat=iostatus) RecoveryFileHeader
 if (iostatus /= 0 ) then
      write (TextOutputFileUnit,1030) 'In subroutine DataIn--Read error, file header both_recov.fil.'
      InputErrorFlag = 1
      return
 end if 

return

1000 format (32x,6(1x,f6.1))
1010 format (a8)
1020 format (a80)
1030 format (a80,/)
end

!*****************************************************************************
subroutine ReadGagingStationDataInput (InputErrorFlag)
!*****************************************************************************
!Subroutine reads gaging-station data from Web-generated input file.

!Definition of local variables:
!-----------------------------------------------------------------------------
!GagingStationNode = variable for the input node of a gaging station that is used to set the appropriate program array value for
!          "MonumentCreekGagingStationNode," "FountainCreekGagingStationNode," or "TributaryGagingStationNode."
!WebinputGagingStationIdentifier = data type identifier for each line in input file, "Webin.fil."
!WebinputLineSequenceNumber = sequence number of each line in input file, "Webin.fil."
!*****************************************************************************
use CommonVariables, &
     only : NumberOfFountainCreekGagingStations,USGSGagingStationNumber_Mainstem,USGSGagingStationName_Mainstem, &
     GagingStationDischarge_Mainstem,GagingStationDischarge_Tributary,TributaryGagingStationNode, &
     NumberOfFountainCreekSegments, USGSGagingStationNumber_Tributary,USGSGagingStationName_Tributary, &
     NumberOfMonumentCreekGagingStations,NumberOfMonumentCreekSegments,NumberOfTributaryGagingStations, &
     TextOutputFileUnit,WebInputFileUnit,MonumentCreekGagingStationNode,FountainCreekGagingStationNode, &
     NumberFountainCreekGages, NumberTributaryGages, NumberMonumentCreekGages, &
     SCRATCH1FileUnit, iostatus
implicit none

integer, intent(out) :: InputErrorFlag
      
integer :: GagingStationNode,CheckForUpstreamFromWWTF,CurrentNumberOfMainstemGagingStations,CurrentGagedFlow
character :: WebinputGagingStationIdentifier*2, WebinputLineSequenceNumber*3
!# Character variable for initial input read
character(80) :: ReadLine

allocate (character(8) :: USGSGagingStationNumber_Mainstem(NumberFountainCreekGages+NumberMonumentCreekGages), &
     USGSGagingStationNumber_Tributary(NumberTributaryGages))
     
allocate (character(40) :: USGSGagingStationName_Mainstem(NumberFountainCreekGages+NumberMonumentCreekGages), &
     USGSGagingStationName_Tributary(NumberTributaryGages))
     

!Input names and discharges at the main-stem gaging stations.
!Set dimensions for number of stations and stream segments in the
!Monument and Fountain Creek portions of the complete study reach.
!Write data to scratch file as read for later writing to saved files.

InputErrorFlag=0
CheckForUpstreamFromWWTF=0
CurrentNumberOfMainstemGagingStations=0
NumberOfMonumentCreekGagingStations=0
NumberOfFountainCreekGagingStations=0
  
!#Read up to 100 stream gaging station records from Webin.fil, then write to the scratch1 file.
LoopThroughAndReadMainstemGagedFlows: do CurrentGagedFlow=1,100

     read (WebInputFileUnit,2222,iostat=iostatus) ReadLine
     WebinputGagingStationIdentifier = ReadLine(:2)
         
     ! read error
     if (iostatus /= 0 ) then
          write (TextOutputFileUnit,1030) 'In subroutine ReadGagingStaionData--Read error for gaging stations data.'
          InputErrorFlag = 1
          return
     end if
      
     !#When the EN delimiter is read, write to the scratch1 file and move on to reading tributary gage data.
     CheckForMainstemGagedFlow: if (WebinputGagingStationIdentifier == 'EN') then
          write (SCRATCH1FileUnit,1040) 'EN99'
          goto 20

     !#If the prefix is 'ST', a main stem stream gaging station has been encountered.                                 
     else if (WebinputGagingStationIdentifier == 'ST') then
          read (ReadLine,1010,iostat=iostatus) &
               WebinputGagingStationIdentifier,WebinputLineSequenceNumber,GagingStationNode, &
               USGSGagingStationNumber_Mainstem(CurrentGagedFlow),USGSGagingStationName_Mainstem(CurrentGagedFlow), &
               GagingStationDischarge_Mainstem(CurrentGagedFlow)
          ! read error
          if (iostatus /= 0 ) then
               write (TextOutputFileUnit,1030) 'In subroutine ReadGagingStaionData--Read error for gaging stations data.'
               InputErrorFlag = 1
               return
          end if
           
          write (SCRATCH1FileUnit,1020) &
               WebinputGagingStationIdentifier,WebinputLineSequenceNumber,GagingStationNode, &
               USGSGagingStationNumber_Mainstem(CurrentGagedFlow),USGSGagingStationName_Mainstem(CurrentGagedFlow), &
               GagingStationDischarge_Mainstem(CurrentGagedFlow)
        
          !#Increment the number of mainstem gaging stations.
          CurrentNumberOfMainstemGagingStations=CurrentNumberOfMainstemGagingStations+1
          !#If the gaging station is upgradient of the Las Vegas WWTF,set Monument Creek station node MonumentCreekGagingStationNode to GagingStationNode.
          CheckForGagingStationUpstreamFromWWTF: if (USGSGagingStationNumber_Mainstem(CurrentGagedFlow) == '07105500') then
               MonumentCreekGagingStationNode(CurrentGagedFlow)=GagingStationNode
               NumberOfMonumentCreekGagingStations=CurrentGagedFlow
               NumberOfMonumentCreekSegments=NumberOfMonumentCreekGagingStations-1 !#Resets the node counter to 1 at the confluence for Fountain Creek nodes.
               CheckForUpstreamFromWWTF=1
          end if CheckForGagingStationUpstreamFromWWTF
          CheckFountainCreekIndex: if (CheckForUpstreamFromWWTF == 0) then
               MonumentCreekGagingStationNode(CurrentGagedFlow)=GagingStationNode
               !#If the gaging station is located below the confluence, the index (CurrentGagedFlow-NumberOfMonumentCreekSegments) of the Fountain Creek node FountainCreekGagingStationNode 
               !#is referenced to the index of confluence station 07105500, which has a Fountain Creek index of 1.                 
          else if(CheckForUpstreamFromWWTF == 1) then  
               FountainCreekGagingStationNode(CurrentGagedFlow-NumberOfMonumentCreekSegments)=GagingStationNode
          end if CheckFountainCreekIndex

     end if CheckForMainstemGagedFlow
end do LoopThroughAndReadMainstemGagedFlows

!#NumberOfFountainCreekGagingStations and NumberOfFountainCreekSegments are never used again in this subroutine, but are reset. 
20 NumberOfFountainCreekGagingStations=CurrentNumberOfMainstemGagingStations-NumberOfMonumentCreekGagingStations+1
   NumberOfFountainCreekSegments=NumberOfFountainCreekGagingStations-1

!Input names and discharges for tributary stations.
NumberOfTributaryGagingStations=0
  
!#Read up to 100 tributary gaging station records from Webin.fil, then write to the scratch1 file.
LoopThroughAndReadTributaryGagedFlows: do CurrentGagedFlow=1,100

     read (WebInputFileUnit,2222,iostat=iostatus) ReadLine
     WebinputGagingStationIdentifier = ReadLine(:2)
      

     ! read error
     if (iostatus /= 0 ) then
          write (TextOutputFileUnit,1030) 'In subroutine ReadGagingStationDataInput--Read error for tributary stations data.'
          InputErrorFlag = 1
          return
     end if
      
     !#When the EN delimiter is read, write to the scratch1 file and return to calling procedure.              
     IfTributaryGagedFlows: if(WebinputGagingStationIdentifier == 'EN') then
          write (SCRATCH1FileUnit,1040) 'EN99'
          return
     
     !#If the prefix is 'TR', a tributary stream gaging station has been encountered.  
      else if (WebinputGagingStationIdentifier == 'TR') then
          read (ReadLine,1010,iostat=iostatus) &
               WebinputGagingStationIdentifier,WebinputLineSequenceNumber, &
               TributaryGagingStationNode(CurrentGagedFlow), &
               USGSGagingStationNumber_Tributary(CurrentGagedFlow), &
               USGSGagingStationName_Tributary(CurrentGagedFlow), &
               GagingStationDischarge_Tributary(CurrentGagedFlow)
                 
          ! read error
          if (iostatus /= 0 ) then
               write (TextOutputFileUnit,1030) &
               'In subroutine ReadGagingStationDataInput--Read error for tributary stations data.'
               InputErrorFlag = 1
               return
           end if
           
            write (SCRATCH1FileUnit,1020) &
               WebinputGagingStationIdentifier,WebinputLineSequenceNumber, &
               TributaryGagingStationNode(CurrentGagedFlow), &
               USGSGagingStationNumber_Tributary(CurrentGagedFlow), &
               USGSGagingStationName_Tributary(CurrentGagedFlow), &
               GagingStationDischarge_Tributary(CurrentGagedFlow)
          !#Increment the number of tributary gaging stations.
          NumberOfTributaryGagingStations=NumberOfTributaryGagingStations + 1
     
     end if IfTributaryGagedFlows
end do LoopThroughAndReadTributaryGagedFlows

return

2222 FORMAT (A80)
  
1010 format (a2,a3,1x,i2,1x,a8,1x,a40,1x,f7.2)
1020 format (a2,a3,';',i2,';',a8,';',a40,';',f7.2)
1030 format (a80,/)
1040 format (a4)
end

!*****************************************************************************
subroutine ReadReturnFlowInput (FryArkWater,ExchangableWater,InputErrorFlag)
!*****************************************************************************
!Subroutine reads all types of native and reusable return flow data.
!Also reads diversions (off-stream storage) of reusable return flows.

!Definition of local variables:
!-----------------------------------------------------------------------------
!ExchangableWater = the amount of colorado springs transmountain return flow water that is available for diversion/sale to native-flow diverters.
!FryArkWater = the amount of colorado springs fry-ark transmountain return flow water that is available for diversion/sale to native-flow diverters.
!WebinputReturnFlowIdentifier = data type identifier for each line in input file, "Webin.fil."
!WebinputLineSequenceNumber = sequence number of each line in input file, "Webin.fil."
!*****************************************************************************
use CommonVariables, &
     only : NativeReturnFlowReleaseNode,ReusableReturnFlowReleaseNode,ReusableReturnFlowDiversionNode, &
     NumberOfNativeReturnFlows,NumberOfReusableReturnFlowDiversions,NumberOfReusableReturnFlows, &
     SurrogateCSFryArkWWTFReturnFlowRelease,SurrogateCSReleaseFromUpstreamReservoir,SurrogateFortCarsonReturnFlowRelease, &
     SurrogateFutureCSWWTFReturnFlowRelease,SurrogateWWTFReturnFlowRelease,NativeReturnFlowReleaseNode, &
     ReleasedNativeReturnFlowName,ReusableReturnFlowReleaseNode,ReleasedReusableReturnFlowName,ReusableReturnFlowDischarge, &
     ReusableReturnFlowDeliveryNode,ReusableReturnFlowDischarge,ReusableReturnFlowDiversionNode,DivertedReusableReturnFlowName, &
     ReusableDiversionDischarge,ReusableReturnFlowEntity,NativeReturnFlowDischarge,TemporaryReusableDiversionDischarge, &
     TextOutputFileUnit,SCRATCH1FileUnit,WebInputFileUnit, iostatus
implicit none
   

integer, intent(out) :: InputErrorFlag
real :: ExchangableWater,FryArkWater

integer :: CurrentReturnFlowRelease

character :: WebinputReturnFlowIdentifier*2,WebinputLineSequenceNumber*3
!# Krammes Alpha variable for initial input read
character(80) :: ReadLine




!input any measured native return flows along mon. and fountain creeks.
!write data to scratch file as read for later writing to files.

InputErrorFlag=0
NumberOfNativeReturnFlows=0

!#Read up to 100 additional records for native flow releases from Webin.fil, then write to the scratch1 file.
LoopThroughAndReadNativeReturnFlowReleases: do CurrentReturnFlowRelease=1,100

     read (WebInputFileUnit,2222,iostat=iostatus) ReadLine
     WebinputReturnFlowIdentifier = ReadLine(:2)
      
     ! read error
     if (iostatus /= 0 ) then
          write (TextOutputFileUnit,1060) 'In subr ReadReturnFlowInput--Read error for native return flow inputs.'
          InputErrorFlag = 1
          return
     end if

     !#If you've read the delimiter 'EN', write to the scratch1 file and start reading the reusable return flow releases.
     CheckForNativeReturnFlowRelease: if (WebinputReturnFlowIdentifier == 'EN') then
          write (SCRATCH1FileUnit,1070) 'EN99'
          goto 20

     !#If the prefix on the record is 'NR', then write to the scratch1 file and increment the number of native return flow releases.
     else if (WebinputReturnFlowIdentifier == 'NR') then
          read (ReadLine,1000,iostat=iostatus) &
               WebinputReturnFlowIdentifier,WebinputLineSequenceNumber, &
               NativeReturnFlowReleaseNode(CurrentReturnFlowRelease), ReleasedNativeReturnFlowName(CurrentReturnFlowRelease), &
               NativeReturnFlowDischarge(CurrentReturnFlowRelease)
                 
          ! read error
          if (iostatus /= 0 ) then
               write (TextOutputFileUnit,1060) 'In subr ReadReturnFlowInput--Read error for native return flow inputs.'
               InputErrorFlag = 1
               return
          end if

          write (SCRATCH1FileUnit,1010) &
               WebinputReturnFlowIdentifier,WebinputLineSequenceNumber,NativeReturnFlowReleaseNode(CurrentReturnFlowRelease), &
               ReleasedNativeReturnFlowName(CurrentReturnFlowRelease),NativeReturnFlowDischarge(CurrentReturnFlowRelease)
          NumberOfNativeReturnFlows=NumberOfNativeReturnFlows+1
     
     end if CheckForNativeReturnFlowRelease
end do LoopThroughAndReadNativeReturnFlowReleases

!Input reusable return flow names and data values along Monument and Fountain Creeks, including all Colorado Springs return flows at
!Northern Reclamation Facility and Las Vegas St. WWTF.

20 NumberOfReusableReturnFlows=0

!#Read up to 500 additional records for reusable return flow releases from Webin.fil, then write to the scratch1 file.
LoopThroughAndReadReusableReturnFlowReleases: do CurrentReturnFlowRelease=1,500

     read (WebInputFileUnit,2222,iostat=iostatus) ReadLine
     WebinputReturnFlowIdentifier = ReadLine(:2)
                      
     ! read error
     if (iostatus /= 0 ) then
          write (TextOutputFileUnit,1060) 'In subr ReadReturnFlowInput--Read error for reusable return flow inputs.'
          InputErrorFlag = 1
          return
     end if

     !#If you've read the delimiter 'EN', write to the scratch1 file and start reading the reusable return flow diversions.
     CheckForReusableReturnFlowRelease:  if (WebinputReturnFlowIdentifier == 'EN') then
          write (SCRATCH1FileUnit,1070) 'EN99'
          goto 40
     !#If the prefix on the record is 'RR', then write to the scratch1 file and increment the number of reusable return flow releases.
     else if (WebinputReturnFlowIdentifier == 'RR') then
          read (ReadLine,1020,iostat=iostatus) &
               WebinputReturnFlowIdentifier,WebinputLineSequenceNumber,ReusableReturnFlowReleaseNode(CurrentReturnFlowRelease), &
               ReleasedReusableReturnFlowName(CurrentReturnFlowRelease),ReusableReturnFlowDischarge(CurrentReturnFlowRelease), &
               ReusableReturnFlowDeliveryNode(CurrentReturnFlowRelease)
                                     
          ! read error
          if (iostatus /= 0 ) then
               write (TextOutputFileUnit,1060) 'In subr ReadReturnFlowInput--Read error for reusable return flow inputs.'
               InputErrorFlag = 1
               return
          end if
          write (SCRATCH1FileUnit,1030) &
               WebinputReturnFlowIdentifier,WebinputLineSequenceNumber,ReusableReturnFlowReleaseNode(CurrentReturnFlowRelease), &
               ReleasedReusableReturnFlowName(CurrentReturnFlowRelease),ReusableReturnFlowDischarge(CurrentReturnFlowRelease), &
               ReusableReturnFlowDeliveryNode(CurrentReturnFlowRelease)
          NumberOfReusableReturnFlows=NumberOfReusableReturnFlows+1
     end if CheckForReusableReturnFlowRelease
end do LoopThroughAndReadReusableReturnFlowReleases

!In setting 'ExchangableWater' assume that Colo. Spgs. transmountain from Northern Facility will be added to Colo. Spgs. transmountain from 
!Las Vegas WWTF and be available for use in sale of water to agricultural diversions or others.Also assumes that losses from Northern 
!Facility downstream are minimal.

40 ExchangableWater=0.0
FryArkWater=0.0

!#Set FryArkWater, the amount of fry-ark transmountain return flow available for diversion/sale to  native-flow diverters,
!#equal to the sum of return flow releases at FRY-ARK RF at N. Recl. Facility and FRY-ARK RF at Las Vegas WWTF that were read above.
FryArkWater=(ReusableReturnFlowDischarge(2)+ReusableReturnFlowDischarge(6))

!#Set ExchangableWater, the amount of return flow water available for diversion/sale to native-flow diverters,
!#equal to the sum of the remaining return flow releases that were read above.
ExchangableWater=(ReusableReturnFlowDischarge(1)+ReusableReturnFlowDischarge(4)+ReusableReturnFlowDischarge(5)+ &
     ReusableReturnFlowDischarge(8))

!Set variable and value for Colo. Springs reusable return flows input at Las Vegas St. WWTF.
SurrogateCSReleaseFromUpstreamReservoir=ReusableReturnFlowDischarge(4)
SurrogateWWTFReturnFlowRelease=ReusableReturnFlowDischarge(5)
SurrogateCSFryArkWWTFReturnFlowRelease=ReusableReturnFlowDischarge(6)
SurrogateFutureCSWWTFReturnFlowRelease=ReusableReturnFlowDischarge(7)
SurrogateFortCarsonReturnFlowRelease=ReusableReturnFlowDischarge(8)

!Input removal/delivery/exchange of any reusable water entities, such as diversion to off-stream storage. Adjust values for "FryArkWater" 
!and "ExchangableWater" used to check transmountain diversions later. Set an additional variable (TemporaryReusableDiversionDischarge) for the reusable diversion that 
!can be adjusted later if the reusable flow for the source entity is less than the specified diversion.
NumberOfReusableReturnFlowDiversions=0

!#Read up to 100 additional records for reusable return flow diversions from Webin.fil, then write to the scratch1 file.
LoopThroughAndReadReusableReturnFlowDiversions: do CurrentReturnFlowRelease=1,100

     read (WebInputFileUnit,2222,iostat=iostatus) ReadLine
     WebinputReturnFlowIdentifier = ReadLine(:2)
                                     
     ! read error
     if (iostatus /= 0 ) then
          write (TextOutputFileUnit,1060) 'In subr ReadReturnFlowInput--Read error, reusable return flow diversions.'
          InputErrorFlag = 1
          return
     end if
           
     !#If you've read the delimiter 'EN', write to the scratch1 file and exit subroutine.
     CheckForReusableReturnFlowDiversion: if (WebinputReturnFlowIdentifier == 'EN') then
          write (SCRATCH1FileUnit,1070) 'EN99'
          goto 60
            
     !#If the prefix on the record is 'RO', then write to the scratch1 file and increment the number of reusable return flow diversions.
     else if (WebinputReturnFlowIdentifier == 'RO') then
          read (ReadLine,1040,iostat=iostatus) &
               WebinputReturnFlowIdentifier,WebinputLineSequenceNumber, &
               ReusableReturnFlowDiversionNode(CurrentReturnFlowRelease), &
               DivertedReusableReturnFlowName(CurrentReturnFlowRelease), &
               ReusableDiversionDischarge(CurrentReturnFlowRelease), &
               ReusableReturnFlowEntity(CurrentReturnFlowRelease)
                                     
          ! read error
          if (iostatus /= 0 ) then
               write (TextOutputFileUnit,1060) 'In subr ReadReturnFlowInput--Read error, reusable return flow diversions.'
               InputErrorFlag = 1
               return
          end if
           
          write (SCRATCH1FileUnit,1050) &
               WebinputReturnFlowIdentifier,WebinputLineSequenceNumber, &
               ReusableReturnFlowDiversionNode(CurrentReturnFlowRelease),&
               DivertedReusableReturnFlowName(CurrentReturnFlowRelease), &
               ReusableDiversionDischarge(CurrentReturnFlowRelease), &
               ReusableReturnFlowEntity(CurrentReturnFlowRelease)
          !#Subtract direct diversions/exchanges (take-outs) of reusable return flows from sum of reusable return flow releases.
          if (ReusableReturnFlowEntity(CurrentReturnFlowRelease) == 6) &
               FryArkWater=FryArkWater-ReusableDiversionDischarge(CurrentReturnFlowRelease)
          if (ReusableReturnFlowEntity(CurrentReturnFlowRelease) == 5) &
               ExchangableWater=ExchangableWater-ReusableDiversionDischarge(CurrentReturnFlowRelease)
          TemporaryReusableDiversionDischarge(CurrentReturnFlowRelease)=ReusableDiversionDischarge(CurrentReturnFlowRelease)
          NumberOfReusableReturnFlowDiversions=NumberOfReusableReturnFlowDiversions+1
     
     end if CheckForReusableReturnFlowDiversion 
end do LoopThroughAndReadReusableReturnFlowDiversions
60 return


2222 FORMAT (A80)

1000 format (a2,a3,1x,i2,1x,a56,1x,f6.2)
1010 format (a2,a3,';',i2,';',a56,';',f6.2)
1020 format (a2,a3,1x,i2,1x,a56,1x,f8.4,1x,i2)
1030 format (a2,a3,';',i2,';',a56,';',f8.4,';',i2)
1040 format (a2,a3,1x,i2,1x,a56,1x,f6.2,1x,i2)
1050 format (a2,a3,';',i2,';',a56,';',f6.2,';',i2)
1060 format (a100)
1070 format (a4)
end

!*****************************************************************************
subroutine ReadDiversionDataInput (NumberOfNativeDiversionDitchesOnFountainCreek,InputErrorFlag)
!*****************************************************************************
!Subroutine reads input data for native diversions and diversions of Colorado Springs transmountain water. Also reads any remarks for output.

!Definition of local variables:
!-----------------------------------------------------------------------------
!WebinputDiversionIdentifier = Data type identifier for each line in input file, "Webin.fil."
!WebinputLineSequenceNumber = Sequence number of each line in input file, "Webin,fil."
!DitchCounter = Counter to sequence the ditches in the "FountainCreekSEExchangeDitch" array.
!NumberOfNativeDiversionDitchesOnFountainCreek = Number of native diversions along Fountain Creek (same as NumberOfFountainCreekDiversionDitches, but local)
!FountainCreekSEExchangeDitch = The number for any of the eight ditches along Fountain Creek for which transit loss savings will be computed when a SE
!     Exchange diversion is used. The ditches are: Chilcotte (4), Liston & Love N. (6), Liston & Love S. (7), Talcott 
!     Cotton. (11), Tom Wanless (12), Burke (15),Toof & Harmon (16), and Sutherland (20).
!*****************************************************************************
use CommonVariables, &
     only : MonumentCreekDiversionNode,DivertedNativeReturnFlowNameOnMonumentCreek, &
     NativeFlowDiversionAlongMonumentCreek,FountainCreekDiversionNode,DivertedNativeReturnFlowNameOnFountainCreek, &
     NativeFlowDiversionAlongFountainCreek,FryArkFlowDiversionAlongFountainCreek,ExchangeDiversion1AlongFountainCreek, &
     FlagThatTurnsOnBalancingAccount,CSRRFDiversion2AlongFountainCreek,UserCommentFromWebInput,BalanceAccountFlag, &
     FlagForExchangeDiversion,FlagForFryArkDiversion,FlagForRRFDiversion,FlagToComputeNetGainWater, &
     NumberOfFountainCreekDiversionDitches,NumberOfMonumentCreekDiversionDitches,MonumentCreekDiversionNode, &
     TransitLossSavingsDiversion,FlagRRFDiversionFromBalancingAccount,TextOutputFileUnit,WebInputFileUnit, &
     MaximumNumberOfLinesUserInput, SCRATCH1FileUnit, iostatus
implicit none
integer, intent(out) :: InputErrorFlag
integer, intent(inout) :: NumberOfNativeDiversionDitchesOnFountainCreek
integer :: DitchCounter,FountainCreekSEExchangeDitch(8),CurrentDiversion,CurrentUserComment

character :: WebinputDiversionIdentifier*2,WebinputLineSequenceNumber*3
!# Krammes Alpha variable for initial input read
character(80) :: ReadLine

allocate (character(1) :: FlagThatTurnsOnBalancingAccount(50))
allocate (character(2) :: FlagRRFDiversionFromBalancingAccount(50))
allocate (character(73) :: UserCommentFromWebInput(MaximumNumberOfLinesUserInput))


!Initialize local variables.
data FountainCreekSEExchangeDitch /4,6,7,11,12,15,16,20/

!Input the native diversions on Monument Creek.
!Write data to scratch file as read for later writing to files.

!Set input error and number of Monument Creek ditches to 0.
InputErrorFlag=0
NumberOfMonumentCreekDiversionDitches=0
FlagRRFDiversionFromBalancingAccount=''

!#Read up to 100 additional records for Monument Creek diversions from Webin.fil, then write to the scratch1 file.
LoopThroughAndReadMonumentCreekDiversions: do CurrentDiversion=1,100
     
     read (WebInputFileUnit,2222,iostat=iostatus) ReadLine
     WebinputDiversionIdentifier = ReadLine(:2)
                                     
     ! read error
     if (iostatus /= 0 ) then
          write (TextOutputFileUnit,1050) 'In subr. ReadDiversionDataInput--read error for monument creek diversions.'
          InputErrorFlag = 1
          return
     end if
      
     !#If you've read the delimiter 'EN', write to the scratch1 file and start reading the Fountain Creek diversions.
     CheckForMonumentCreekDiversion: if (WebinputDiversionIdentifier == 'EN') then
          write (SCRATCH1FileUnit,1060) 'EN99'
          goto 20
     !#If the prefix on the record is 'MD', then write to the scratch1 file and increment the number of Monument Creek diversions (ditches).
     else if (WebinputDiversionIdentifier == 'MD') then
          read (ReadLine,1000,iostat=iostatus) &
               WebinputDiversionIdentifier,WebinputLineSequenceNumber, &
               MonumentCreekDiversionNode(CurrentDiversion), DivertedNativeReturnFlowNameOnMonumentCreek(CurrentDiversion), &
               NativeFlowDiversionAlongMonumentCreek(CurrentDiversion)
                                               
          ! read error
          if (iostatus /= 0 ) then
               write (TextOutputFileUnit,1050) 'In subr. ReadDiversionDataInput--read error for monument creek diversions.'
               InputErrorFlag = 1
               return
          end if
           
          write (SCRATCH1FileUnit,1010) &
               WebinputDiversionIdentifier,WebinputLineSequenceNumber,MonumentCreekDiversionNode(CurrentDiversion), &
               DivertedNativeReturnFlowNameOnMonumentCreek(CurrentDiversion), &
               NativeFlowDiversionAlongMonumentCreek(CurrentDiversion)
          NumberOfMonumentCreekDiversionDitches=NumberOfMonumentCreekDiversionDitches+1
     
     end if CheckForMonumentCreekDiversion
end do LoopThroughAndReadMonumentCreekDiversions

!Input the native and transmountain diversions on Fountain Creek.Set flags for any transmountain diversions.
!Set number of Fountain Creek ditches to 0.
20 NumberOfFountainCreekDiversionDitches=0

!Set all flags to FALSE.
FlagForFryArkDiversion=.FALSE.
FlagForExchangeDiversion=.FALSE.
FlagToComputeNetGainWater=.FALSE.
BalanceAccountFlag=.FALSE.
FlagForRRFDiversion=.FALSE.
DitchCounter=1

!#Read up to 100 additional records for Fountain Creek diversions from Webin.fil, then write to the scratch1 file.
LoopThroughAndReadFountainCreekDiversions: do CurrentDiversion=1,100
         
     read (WebInputFileUnit,2222,iostat=iostatus) ReadLine
                                
     ! read error
     if (iostatus /= 0 ) then
          write (TextOutputFileUnit,1050) 'In subr. ReadDiversionDataInput--read error for fountain creek diversions.'
          InputErrorFlag = 1
          return
     end if
      
      WebinputDiversionIdentifier = ReadLine(:2)
      
       !#If you've read the delimiter 'EN', write to the scratch1 file and Read remarks
     CheckForFountainCreekDiversion: if (WebinputDiversionIdentifier == 'EN') then
          write (SCRATCH1FileUnit,1060) 'EN99'
          NumberOfNativeDiversionDitchesOnFountainCreek=NumberOfFountainCreekDiversionDitches
          exit
     
     !#If the prefix on the record is 'FD', then write to the scratch1 file and increment the number of Fountain Creek diversions (ditches).
     
      else if (WebinputDiversionIdentifier == 'FD') then
          
          read (ReadLine,1020,iostat=iostatus) &
               WebinputDiversionIdentifier,WebinputLineSequenceNumber, &
               FountainCreekDiversionNode(CurrentDiversion), &
               DivertedNativeReturnFlowNameOnFountainCreek(CurrentDiversion), &
               NativeFlowDiversionAlongFountainCreek(CurrentDiversion), &
               FryArkFlowDiversionAlongFountainCreek(CurrentDiversion), &
               ExchangeDiversion1AlongFountainCreek(CurrentDiversion), &
               FlagThatTurnsOnBalancingAccount(CurrentDiversion), &
               CSRRFDiversion2AlongFountainCreek(CurrentDiversion)
                                               
          ! read error
          if (iostatus /= 0 ) then
               write (TextOutputFileUnit,1050) 'In subr. ReadDiversionDataInput--read error for fountain creek diversions.'
               InputErrorFlag = 1
               return
          end if
     
          write (SCRATCH1FileUnit,1030) &
               WebinputDiversionIdentifier,WebinputLineSequenceNumber,FountainCreekDiversionNode(CurrentDiversion), &
               DivertedNativeReturnFlowNameOnFountainCreek(CurrentDiversion), &
               NativeFlowDiversionAlongFountainCreek(CurrentDiversion), &
               FryArkFlowDiversionAlongFountainCreek(CurrentDiversion), &
               ExchangeDiversion1AlongFountainCreek(CurrentDiversion), &
               FlagThatTurnsOnBalancingAccount(CurrentDiversion), CSRRFDiversion2AlongFountainCreek(CurrentDiversion)
          
          !Set transmountain diversion flags if diversion is greater than 0 cfs.
          if (FryArkFlowDiversionAlongFountainCreek(CurrentDiversion) > 0.0) FlagForFryArkDiversion=.TRUE.
          if (ExchangeDiversion1AlongFountainCreek(CurrentDiversion) > 0.0) FlagForExchangeDiversion=.TRUE.
          if (CSRRFDiversion2AlongFountainCreek(CurrentDiversion) > 0.0) FlagForRRFDiversion=.TRUE.
          
          !If the user input upper case 'Y' or 'N', reset to lower case.
          if (FlagThatTurnsOnBalancingAccount(CurrentDiversion) == 'Y') FlagThatTurnsOnBalancingAccount(CurrentDiversion)='y'
          if (FlagThatTurnsOnBalancingAccount(CurrentDiversion) == 'N') FlagThatTurnsOnBalancingAccount(CurrentDiversion)='n'
       
       !Set value for SE Exchange diversion that will be used in transit loss savings computations.
          CheckForSEExchangeDiversion: if (ExchangeDiversion1AlongFountainCreek(CurrentDiversion) > 0.0) then
          !!! .and. i == FountainCreekSEExchangeDitch(DitchCounter)) then
          select case (CurrentDiversion)
               case (4,6,7,11,12,15,16,20)
                 
                 !Not from balance account, so included in TL savings computation.
               CheckIfFromBalanceAccount: if (FlagThatTurnsOnBalancingAccount(CurrentDiversion) == 'n') then
                    TransitLossSavingsDiversion(FountainCreekSEExchangeDitch(DitchCounter)) = &
                    ExchangeDiversion1AlongFountainCreek(CurrentDiversion)
                    !Set output key if diversion is used in TL savings computation.
                    FlagRRFDiversionFromBalancingAccount(CurrentDiversion)='_N'
                    !Set flag to call transit loss savings subroutine.
                    FlagToComputeNetGainWater=.TRUE.
               else if (FlagThatTurnsOnBalancingAccount(CurrentDiversion) == 'y') then
                    !From bal. account, so not included in tl savings computation. Set output key if diversion is from balancing account.
                    FlagRRFDiversionFromBalancingAccount(CurrentDiversion)='_B'
                    !Set flag to call WriteTransitLossOutput subroutine to output balance account results.
                    if (.not. FlagToComputeNetGainWater) BalanceAccountFlag=.TRUE.
               end if CheckIfFromBalanceAccount
               DitchCounter = DitchCounter + 1
             end select
          end if CheckForSEExchangeDiversion

          !Set output key for Greenview ditch if diversion is from balancing account.
          CheckIfGreenviewDitch: &
          if (CurrentDiversion == 25 .and. ExchangeDiversion1AlongFountainCreek(CurrentDiversion) > 0.0 .and.  &
               FlagThatTurnsOnBalancingAccount(CurrentDiversion) == 'y') then
                    FlagRRFDiversionFromBalancingAccount(CurrentDiversion)='_B'
                    !Set flag to call WriteTransitLossOutput subroutine to output balance account results when only greenview ditch is being used.
                    if (.not. FlagToComputeNetGainWater) BalanceAccountFlag=.TRUE.
         end if CheckIfGreenviewDitch
         !Increment counter for ditches in transit loss savings.
         NumberOfFountainCreekDiversionDitches=NumberOfFountainCreekDiversionDitches + 1
         
     end if CheckForFountainCreekDiversion
     
     !Increment counter for ditches in transit loss savings.
     !!!!if (CurrentDiversion == FountainCreekSEExchangeDitch(DitchCounter)) DitchCounter=DitchCounter+1
     
end do LoopThroughAndReadFountainCreekDiversions

!Read and write any remarks for output.
40   LoopThroughAndReadUserComments: do CurrentUserComment=1,MaximumNumberOfLinesUserInput
          
          read (WebInputFileUnit,2222,iostat=iostatus) ReadLine
           
          ! read error Get out
          if (iostatus > 0 ) then
               write (TextOutputFileUnit,1050) 'In subroutine DataIn--read error, remarks for output.'
               InputErrorFlag = 1
               return
          end if
          
          read (ReadLine,1040) UserCommentFromWebInput(CurrentUserComment)

        write (SCRATCH1FileUnit,1040) UserCommentFromWebInput(CurrentUserComment)
        if (UserCommentFromWebInput(CurrentUserComment)(1:2) == 'EN') return     !# 'END' Should be the last line if all is well
          
    end do LoopThroughAndReadUserComments

999 return

2222 FORMAT (A80)

1000 format (a2,a3,1x,i2,1x,a26,1x,f6.2)
1010 format (a2,a3,';',i2,';',a26,';',f6.2)
1020 format (a2,a3,1x,i2,1x,a26,3(1x,f6.2),1x,a1,1x,f6.2)
1030 format (a2,a3,';',i2,';',a26,3(';',f6.2),';',a1,';',f6.2)
1040 format (a73)
1050 format (a100)
1060 format (a4)
end

!*****************************************************************************
subroutine CheckForDiversionsExceedingReleasesAndAccountBalance (FryArkWater,ExchangableWater)
!*****************************************************************************
!Subroutine checks to see what the balance (DifferenceBetweenPurchasedAndUsedDiversions) is in the account
!for each ditch (acct.fil) for purposes of diversion of Colorado Springs
!transmountain return flows. Balance is compared to current diversion
!amount; if account is less than diversion, comment written for output.

!Definition of local variables:
!------------------------------------------------------------------------------------------------------------------------------------------
!ExchangableWater = the amount of Colorado Springs transmountain return flow water that is available for diversion/sale to native-flow diverters.
!FryArkWater = the amount of Colorado Springs Fry-Ark transmountain return flow water that is available for diversion/sale to native-flow diverters.
!ExchangableWaterThatCanBeUsedForDiversion = variable for a portion of "ExchangableWater" that can be used for Colorado Springs transmountain diversions.
!AccumulatedFryArkExchangeAndRRFDiversions = variable to accumulate the array values for the three types of potential Colorado Springs transmountain diversions ("FryArkFlowDiversionAlongFountainCreek"
!         "ExchangeDiversion1AlongFountainCreek" "CSRRFDiversion2AlongFountainCreek").
!DifferenceBetweenPurchasedAndUsedDiversions = the difference between the array values for purchased ("*_have") and used ("*_used") values for Colorado Springs transmountain
!          diversions. Used to check against input diversion values.
!******************************************************************************************************************************************
use CommonVariables, &
     only : AcreFeetCorrespondingTo1CFSOverOneDay,FlagForExchangeDiversion,FlagForFryArkDiversion, &
     FlagForRRFDiversion,NumberOfFountainCreekDiversionDitches,FryArkFlowDiversionAlongFountainCreek, &
     AmountOfCSFryArkWaterUsed, FryArkFlowDiversionAlongFountainCreek,DivertedNativeReturnFlowNameOnFountainCreek, &
     FryArkFlowDiversionAlongFountainCreek,DivertedNativeReturnFlowNameOnFountainCreek,ExchangeDiversion1AlongFountainCreek, &
     ExchangeDiversion1AlongFountainCreek,FlagThatTurnsOnBalancingAccount,ExchangeDiversion1AlongFountainCreek, &
     DivertedNativeReturnFlowNameOnFountainCreek, ExchangeDiversion1AlongFountainCreek,CSRRFDiversion2AlongFountainCreek, &
     AmountOfCSWaterUsed,NumberOfFountainCreekDiversionDitches,AmountOfCSFryArkWaterUsed,AmountOfCSExchangeWaterUsed, &
     AmountOfCSWaterAvailable,AmountOfCSFryArkWaterAvailable,AmountOfCSExchangeWaterAvailable,SCRATCH2FileUnit, iostatus
implicit none

real, intent(inout) :: FryArkWater,ExchangableWater
real :: ExchangableWaterThatCanBeUsedForDiversion,AccumulatedFryArkExchangeAndRRFDiversions, &
     DifferenceBetweenPurchasedAndUsedDiversions
      
integer :: CurrentDiversionDitch

!If Fry-Ark diversions, first check if 'purchased' balance < 'used' balance. Compute difference, put in 'accdiff' array. If all used up or if
!diversion > difference, write comment to output. Account check for first 27 Fountain Creek ditches only.
!G.Kuhn, 08/15/09: Modified account check to include all Fountain Creek ditches because new ditches added.

CheckFryArkDiversions: if (FlagForFryArkDiversion) then
     AccumulatedFryArkExchangeAndRRFDiversions=0.0
     LoopThroughFryArkDiversions: do CurrentDiversionDitch=1,NumberOfFountainCreekDiversionDitches
     !set to zero if a negative value appears.
          CheckFryArkDiversion: if (FryArkFlowDiversionAlongFountainCreek(CurrentDiversionDitch) <= 0.0) then
               FryArkFlowDiversionAlongFountainCreek(CurrentDiversionDitch)=0.0
          else if (FryArkFlowDiversionAlongFountainCreek(CurrentDiversionDitch) > 0.0) then
               !Check internal program account balance.
               DifferenceBetweenPurchasedAndUsedDiversions=(AmountOfCSFryArkWaterAvailable(CurrentDiversionDitch) - &
                    AmountOfCSFryArkWaterUsed(CurrentDiversionDitch))/1.9835
               if (DifferenceBetweenPurchasedAndUsedDiversions <= 0.0 .or.  &
                    DifferenceBetweenPurchasedAndUsedDiversions < FryArkFlowDiversionAlongFountainCreek(CurrentDiversionDitch))  &
                    write (SCRATCH2FileUnit,1000) DivertedNativeReturnFlowNameOnFountainCreek(CurrentDiversionDitch)
               !Check diversion total with available return flow.
               AccumulatedFryArkExchangeAndRRFDiversions=AccumulatedFryArkExchangeAndRRFDiversions+ &
                    FryArkFlowDiversionAlongFountainCreek(CurrentDiversionDitch)
               if (AccumulatedFryArkExchangeAndRRFDiversions > FryArkWater) write (SCRATCH2FileUnit,1010) & 
                    DivertedNativeReturnFlowNameOnFountainCreek(CurrentDiversionDitch)
          end if CheckFryArkDiversion
     end do LoopThroughFryArkDiversions
end if CheckFryArkDiversions

1000 format ('IACHK CSU FRY-ARK account balance smaller than diversion for ',a16)
1010 format ('IACHK CSU FRY-ARK diversions greater than FRY-ARK return flows for ',a16)

!If SE Exchange diversions, first check if 'purchased' balance < 'used' balance. Compute difference, put in 'accdiff' array. If all used up or if
!diversion > difference, write comment to output.
CheckSEExchangeDiversions:  if (FlagForExchangeDiversion) then
     AccumulatedFryArkExchangeAndRRFDiversions=0.0
     LoopThroughSEExchangeDiversions:  do CurrentDiversionDitch=1,NumberOfFountainCreekDiversionDitches
          CheckSEExchangeDiversion: if (ExchangeDiversion1AlongFountainCreek(CurrentDiversionDitch) <= 0.0) then
               ExchangeDiversion1AlongFountainCreek(CurrentDiversionDitch)=0.0
               !Check internal program account balance, but exclude if from bal. account.
          else if (ExchangeDiversion1AlongFountainCreek(CurrentDiversionDitch) > 0.0) then
               if (FlagThatTurnsOnBalancingAccount(CurrentDiversionDitch) == 'n') then
                    DifferenceBetweenPurchasedAndUsedDiversions=(AmountOfCSExchangeWaterAvailable(CurrentDiversionDitch)- &
                         AmountOfCSExchangeWaterUsed(CurrentDiversionDitch))/AcreFeetCorrespondingTo1CFSOverOneDay
                    if (DifferenceBetweenPurchasedAndUsedDiversions <= 0.0 .or. &
                         DifferenceBetweenPurchasedAndUsedDiversions < &
                         ExchangeDiversion1AlongFountainCreek(CurrentDiversionDitch)) &
                         write (SCRATCH2FileUnit,1020) DivertedNativeReturnFlowNameOnFountainCreek(CurrentDiversionDitch)
               end if
               !Check for total value of SE exchange water includes balance account usage.
               AccumulatedFryArkExchangeAndRRFDiversions=AccumulatedFryArkExchangeAndRRFDiversions+ &
                    ExchangeDiversion1AlongFountainCreek(CurrentDiversionDitch)
               if (AccumulatedFryArkExchangeAndRRFDiversions > ExchangableWater) write (SCRATCH2FileUnit,1030) &
                    DivertedNativeReturnFlowNameOnFountainCreek(CurrentDiversionDitch)
          end if CheckSEExchangeDiversion
      end do LoopThroughSEExchangeDiversions
end if CheckSEExchangeDiversions
1020 format ('IACHK CSU SE EXCH. account balance smaller than diversion for ',a16)
1030 format ('IACHK CSU SE EXCH. diversions greater than transmountain return flows for ',a16)

!If Colorado Springs re-usable diversions, first check if 'purchased' balance < 'used' balance Compute difference, put in 'DifferenceBetweenPurchasedAndUsedDiversions' array.
!If all used up or if diversion > difference, set diversion to 0.
ExchangableWaterThatCanBeUsedForDiversion=(ExchangableWater-AccumulatedFryArkExchangeAndRRFDiversions)
CheckCSRUDiversions: if (FlagForRRFDiversion) then
     AccumulatedFryArkExchangeAndRRFDiversions=0.0
     LoopThroughCSRUDiversions: do CurrentDiversionDitch=1,NumberOfFountainCreekDiversionDitches
          CheckCSRUDiversion: if (CSRRFDiversion2AlongFountainCreek(CurrentDiversionDitch) <= 0.0) then
               CSRRFDiversion2AlongFountainCreek(CurrentDiversionDitch)=0.0
          else if (CSRRFDiversion2AlongFountainCreek(CurrentDiversionDitch) > 0.0) then
               !Check internal program account balance.
               DifferenceBetweenPurchasedAndUsedDiversions = &
                    (AmountOfCSWaterAvailable(CurrentDiversionDitch)-AmountOfCSWaterUsed(CurrentDiversionDitch))/1.9835
               if ((CSRRFDiversion2AlongFountainCreek(CurrentDiversionDitch) > 0.0) .and. &
                    ((DifferenceBetweenPurchasedAndUsedDiversions <= 0.0) .or. &
                    (DifferenceBetweenPurchasedAndUsedDiversions < CSRRFDiversion2AlongFountainCreek(CurrentDiversionDitch)))) &
                    write (SCRATCH2FileUnit,1040) DivertedNativeReturnFlowNameOnFountainCreek(CurrentDiversionDitch)
               !Check diversion total with available return flow.
               AccumulatedFryArkExchangeAndRRFDiversions=AccumulatedFryArkExchangeAndRRFDiversions+ &
                    CSRRFDiversion2AlongFountainCreek(CurrentDiversionDitch)
               if (AccumulatedFryArkExchangeAndRRFDiversions > ExchangableWaterThatCanBeUsedForDiversion) &
                    write (SCRATCH2FileUnit,1050) DivertedNativeReturnFlowNameOnFountainCreek(CurrentDiversionDitch)
          end if CheckCSRUDiversion
     end do LoopThroughCSRUDiversions
end if CheckCSRUDiversions

return

1040 format ('IACHK CSU REUSE ACCOUNT BALANCE smaller than diversion for ',a16)
1050 format ('IACHK CSU REUSE DIVERSIONS greater than TRANSMOUNTAIN RETURN FLOWS for ',a16)
end

!*****************************************************************************
subroutine SetArrayDimensions
!*****************************************************************************
!Subroutine computes remainder of program dimensions that have not been set or computed in previous subroutines. In the program, the only 
!dimension values set internally are the number of nodes (NumberOfMonumentCreekNodes and NumberOfFountainCreekNodes) and the only data values set internally are the river miles 
!(DownstreamDistanceOfNodes) for the 34 nodes. All other dimensions and variables either are read on input or computed within the program.

!Definition of local variables:
!------------------------------------------------------------------------------------------------------------------------------------------
!FountainCreekDiversionDitchesSortedDownstream = an array to VariableHeldForScratchFileInputOutput the "character" value for the downstream sorted node number for ditches along Fountain Creek.
!FountainCreekDiversionDitchNumbers = a temporary character string into which the numbers of Fountain Creek ditches in each subreach (from "FountainCreekDiversionDitchesSortedDownstream" array) are written.
!       Used to fill the "MonumentCreekSubreachDitchNumber" array in "names.cmn" include file.
!FountainCreekDiversionNode2 = the node numbers where native/transmountain diversions are located along Fountain Creek but order is sorted in downstream direction.
!MonumentCreekDiversionDitchesSortedDownstream = an array to VariableHeldForScratchFileInputOutput the "character" value for the downstream sorted node number for ditches along Monument Creek.
!MonumentCreekDiversionDitchNumbers = a temporary character string into which the numbers of Monument Creek ditches in each subreach (from "MonumentCreekDiversionDitchesSortedDownstream" array) are written. 
!       Used to fill the "MonumentCreekSubreachDitchNumber" array in "names.cmn" include file.
!MonumentCreekDiversionNode2 = the node numbers where native/transmountain diversions are located along Monument Creek but order is sorted in downstream direction.
!******************************************************************************************************************************************
use CommonVariables, &
     only : NumberOfFountainCreekDiversionDitches,NumberOfFountainCreekNodes,NumberOfFountainCreekSegments, &
     NumberOfFountainCreekSubreaches,NumberOfMonumentCreekDiversionDitches,NumberOfMonumentCreekNodes, &
     NumberOfMonumentCreekSegments,NumberOfMonumentCreekSubreaches,TotalNumberOfNodes,DownstreamDistanceOfNodes, &
     MonumentCreekSubreachLength,MonumentCreekGagingStationNode,MonumentCreekSegmentLength,MonumentCreekDiversionNode, &
     MonumentCreekSubreachDitchNumber,FountainCreekSubreachLength,FountainCreekGagingStationNode,FountainCreekSegmentLength, &
     FountainCreekDiversionNode,FountainCreekSubreachDitchNumber
implicit none

character :: MonumentCreekDiversionDitchNumbers*17,FountainCreekDiversionDitchNumbers*17, &
     MonumentCreekDiversionDitchesSortedDownstream(30)*2,FountainCreekDiversionDitchesSortedDownstream(50)*2


integer :: MonumentCreekDiversionNode2(30),FountainCreekDiversionNode2(50),CurrentSegment,CurrentNode,CurrentDitch, &
     UpstreamLocation,DownstreamLocation,SortedDitch,CurrentSubreach,FlagDitchInCurrentSubreach,InverseDownstreamLocation, &
     CurrentInverseDownstreamLocation


!Initialize local variables.
data MonumentCreekDiversionNode2 /30*0/,FountainCreekDiversionNode2 /50*0/
data MonumentCreekDiversionDitchesSortedDownstream /30*''/,FountainCreekDiversionDitchesSortedDownstream /50*''/

FountainCreekDiversionDitchNumbers = ''

TotalNumberOfNodes=NumberOfMonumentCreekNodes+NumberOfFountainCreekNodes
NumberOfMonumentCreekSubreaches=NumberOfMonumentCreekNodes-1
NumberOfFountainCreekSubreaches=NumberOfFountainCreekNodes-1

!Set dimensions and values for fixed data for Monument Creek. Set channel length for each subreach (MonumentCreekSubreachLength) and distance between
!stations (MonumentCreekSegmentLength) along the reach--Needed to calculate gain/loss in native streamflow.
LoopThroughAndCalculateLengthsOfMonumentCreekSubreaches:  do CurrentSegment=1,NumberOfMonumentCreekSubreaches
     MonumentCreekSubreachLength(CurrentSegment)=DownstreamDistanceOfNodes(CurrentSegment+1) - &
          DownstreamDistanceOfNodes(CurrentSegment)
end do LoopThroughAndCalculateLengthsOfMonumentCreekSubreaches

LoopThroughAndCalculateLengthsOfMonumentCreekSegments: do CurrentSegment=1,NumberOfMonumentCreekSegments
     UpstreamLocation=MonumentCreekGagingStationNode(CurrentSegment)
     DownstreamLocation=MonumentCreekGagingStationNode(CurrentSegment+1)
     MonumentCreekSegmentLength(CurrentSegment)=DownstreamDistanceOfNodes(DownstreamLocation) - &
          DownstreamDistanceOfNodes(UpstreamLocation)
end do LoopThroughAndCalculateLengthsOfMonumentCreekSegments

!Sort Monument Creek diversions by downstream input node order and set values for sorted order. Assumes no Monument Creek ditches are input
!for any nodes numbered 15 or larger.
SortedDitch=0
LoopThroughAlMonthOfLastReleaseDateumentCreekDitchNodes: do CurrentNode=1,NumberOfMonumentCreekNodes
     LoopThroughAndSortMonumentCreekDiversionDitches: do CurrentDitch=1,NumberOfMonumentCreekDiversionDitches
          CheckForMonumentCreekNodeInSortedNodeArray: if (MonumentCreekDiversionNode(CurrentDitch) == CurrentNode) then
               SortedDitch=SortedDitch+1
               MonumentCreekDiversionNode2(SortedDitch)=MonumentCreekDiversionNode(CurrentDitch)
               !This writes the number of the downstream sorted ditch into the "MonumentCreekDiversionDitchesSortedDownstream" array.
               write (MonumentCreekDiversionDitchesSortedDownstream(SortedDitch),'(i2)') CurrentDitch
          end if CheckForMonumentCreekNodeInSortedNodeArray
     end do LoopThroughAndSortMonumentCreekDiversionDitches
end do LoopThroughAlMonthOfLastReleaseDateumentCreekDitchNodes

!Using the sorted ditch order, determine which ditches are in specific subreaches (MonumentCreekSubreachDitchNumber) for use in printed output.
LoopToFindCurrentMonumentCreekDitchSubreach: do CurrentSubreach=1,NumberOfMonumentCreekSubreaches
     FlagDitchInCurrentSubreach=0
     UpstreamLocation=1
     DownstreamLocation = 0
     MonumentCreekDiversionDitchNumbers(1:17)=''
     LoopThroughAlMonthOfLastReleaseDateumentCreekDiversionDitches: do CurrentDitch=1,NumberOfMonumentCreekDiversionDitches
          CheckForDitchInCurrentMonumentCreekSubreach: if (MonumentCreekDiversionNode2(CurrentDitch) == CurrentSubreach) then
               FlagDitchInCurrentSubreach=1
               DownstreamLocation=UpstreamLocation+2
               !Select appropriate value of MonumentCreekDiversionDitchesSortedDownstream array and write into MonumentCreekDiversionDitchNumbers string. Append comma for next value.
               MonumentCreekDiversionDitchNumbers(UpstreamLocation:DownstreamLocation) = &
                    MonumentCreekDiversionDitchesSortedDownstream(CurrentDitch)//','
               UpstreamLocation=DownstreamLocation+1
          end if CheckForDitchInCurrentMonumentCreekSubreach
     end do LoopThroughAlMonthOfLastReleaseDateumentCreekDiversionDitches
     !Set first ditch number in each MonumentCreekSubreachDitchNumber array element.
     MonumentCreekSubreachDitchNumber(CurrentSubreach)=MonumentCreekDiversionDitchNumbers(1:DownstreamLocation-1)
     !No ditches in subreach.
     CheckForNoMonumentCreekDiversionDitchesInSubreach: if (FlagDitchInCurrentSubreach == 0) then
          MonumentCreekSubreachDitchNumber(CurrentSubreach)='        None'
     else
          InverseDownstreamLocation=nint(float(18-DownstreamLocation)/2.) !Why.
          CheckForNotFirstMonumentCreekDitch: if (InverseDownstreamLocation > 1) then
               LoopThroughDiversionDitchesAndCreateListOfDitchesInSubreach: do CurrentInverseDownstreamLocation= &
                    1,InverseDownstreamLocation+1
                    !Add rest of ditch numbers in subreach.
                    MonumentCreekSubreachDitchNumber(CurrentSubreach)=' '//MonumentCreekSubreachDitchNumber(CurrentSubreach)
               end do LoopThroughDiversionDitchesAndCreateListOfDitchesInSubreach
          end if CheckForNotFirstMonumentCreekDitch
     end if CheckForNoMonumentCreekDiversionDitchesInSubreach
end do LoopToFindCurrentMonumentCreekDitchSubreach

!Set dimensions and values for fixed data for Fountain Creek. Set channel length for each subreach (FountainCreekSubreachLength) and distance between
!stations (FountainCreekSegmentLength) along the reach -- needed to calculate gain/loss in native streamflow.
LoopThroughAndCalculateFountainCreekSubreachLength: do CurrentNode=1,NumberOfFountainCreekNodes
     FountainCreekSubreachLength(CurrentNode)=DownstreamDistanceOfNodes(CurrentNode+NumberOfMonumentCreekSubreaches+1)-&
          DownstreamDistanceOfNodes(CurrentNode+NumberOfMonumentCreekSubreaches)
end do LoopThroughAndCalculateFountainCreekSubreachLength

LoopThroughAndCalculateFountainCreekSegmentLength: do CurrentSegment=1,NumberOfFountainCreekSegments
     UpstreamLocation=FountainCreekGagingStationNode(CurrentSegment)
     DownstreamLocation=FountainCreekGagingStationNode(CurrentSegment+1)
     FountainCreekSegmentLength(CurrentSegment)=DownstreamDistanceOfNodes(DownstreamLocation)- &
          DownstreamDistanceOfNodes(UpstreamLocation)
end do LoopThroughAndCalculateFountainCreekSegmentLength

!Set distance for subreaches in segment beyond last gage.
CheckForFountainCreekEndSegment: if (DownstreamLocation < TotalNumberOfNodes) then
     NumberOfFountainCreekSegments=NumberOfFountainCreekSegments+1
     NumberOfFountainCreekSubreaches=NumberOfFountainCreekSubreaches+1
     FountainCreekSegmentLength(NumberOfFountainCreekSegments)=0.0
     LoopThroughAndCalculateFountainCreekSegmentLengthInEndSegment: do CurrentNode=DownstreamLocation+1,TotalNumberOfNodes
          FountainCreekSegmentLength(NumberOfFountainCreekSegments)=FountainCreekSegmentLength(NumberOfFountainCreekSegments)+ &
               (DownstreamDistanceOfNodes(CurrentNode)-DownstreamDistanceOfNodes(CurrentNode-1))
     end do LoopThroughAndCalculateFountainCreekSegmentLengthInEndSegment
end if CheckForFountainCreekEndSegment

!Sort Fountain Creek diversions by downstream input node order and set values for sorted order.
SortedDitch=0
LoopThroughAllFountainCreekDitchNodes: do CurrentNode=1,NumberOfFountainCreekNodes+NumberOfMonumentCreekNodes
     LoopThroughAndSortFountainCreekDiversionDitches: do CurrentDitch=1,NumberOfFountainCreekDiversionDitches
          CheckForFountainCreekNodeInSortedNodeArray: if (FountainCreekDiversionNode(CurrentDitch) == CurrentNode) then
               SortedDitch=SortedDitch+1
               FountainCreekDiversionNode2(SortedDitch)=FountainCreekDiversionNode(CurrentDitch)
               !this writes the number of the downstream sorted ditch into the "FountainCreekDiversionDitchesSortedDownstream" array.
               write (FountainCreekDiversionDitchesSortedDownstream(SortedDitch),'(i2)') CurrentDitch
          end if CheckForFountainCreekNodeInSortedNodeArray
     end do LoopThroughAndSortFountainCreekDiversionDitches
end do LoopThroughAllFountainCreekDitchNodes

!Determine which ditches are in specific subreaches (FountainCreekSubreachDitchNumber) for use in printed output.
LoopToFindCurrentFountainCreekDitchSubreach: do CurrentSubreach=1,NumberOfFountainCreekSubreaches
     FlagDitchInCurrentSubreach=0
     UpstreamLocation=1
     !DownstreamLocation = 0
     FountainCreekDiversionDitchNumbers(1:17)=''

     LoopThroughAllFountainCreekDiversionDitches: do CurrentDitch=1,NumberOfFountainCreekDiversionDitches
        
          CheckForDitchInCurrentFountainCreekSubreach: if (FountainCreekDiversionNode2(CurrentDitch) == &
               CurrentSubreach+NumberOfMonumentCreekSubreaches) then
              
               FlagDitchInCurrentSubreach=1
               DownstreamLocation=UpstreamLocation+2
               !Select appropriate value of FountainCreekDiversionDitchesSortedDownstream array and write into FountainCreekDiversionDitchNumbers string. Append comma for next value.
               FountainCreekDiversionDitchNumbers(UpstreamLocation:DownstreamLocation) = &     
                    FountainCreekDiversionDitchesSortedDownstream(CurrentDitch)//','
               UpstreamLocation=DownstreamLocation+1
          end if CheckForDitchInCurrentFountainCreekSubreach
     end do LoopThroughAllFountainCreekDiversionDitches
       
     !Set first ditch number in each FountainCreekSubreachDitchNumber array.
     ! FountainCreekSubreachDitchNumber(CurrentSubreach) = FountainCreekDiversionDitchNumbers(1:DownstreamLocation-1)  !Error exceeding size of string on first time through loop
     !# krammes.  Not best solution, but need to trap overrun string size on first time through loop. To match F77 version. fix logic later?  Need trap that DownstreamLocation
     !#
     if (DownstreamLocation > 17) then 
          FountainCreekSubreachDitchNumber(CurrentSubreach) = ''
     else 
          FountainCreekSubreachDitchNumber(CurrentSubreach) = FountainCreekDiversionDitchNumbers(1:DownstreamLocation-1)
     end if
          
     CheckForNoFountainCreekDiversionDitchesInSubreach: if (FlagDitchInCurrentSubreach == 0) then
          !No ditches in subreach.
          FountainCreekSubreachDitchNumber(CurrentSubreach)='        None'
     else          
          InverseDownstreamLocation=nint(float(18-DownstreamLocation)/2.)
          
          CheckForNotFirstFountainCreekDitch: if (InverseDownstreamLocation > 1) then
               CreateListOfAllFountainCreekDiversionDitchesInSubreach: &
               do CurrentInverseDownstreamLocation=1,InverseDownstreamLocation+1
                    !Add rest of ditch numbers in subreach.
                    FountainCreekSubreachDitchNumber(CurrentSubreach)=' '//FountainCreekSubreachDitchNumber(CurrentSubreach)
               end do CreateListOfAllFountainCreekDiversionDitchesInSubreach
          end if CheckForNotFirstFountainCreekDitch
          
     end if CheckForNoFountainCreekDiversionDitchesInSubreach
end do LoopToFindCurrentFountainCreekDitchSubreach

return
end

!*****************************************************************************
subroutine SumDiversions
!*****************************************************************************
!Subroutine sums the native and transmountain diversions within each stream segment (between the gages) and within each subreach.

!Definition of local variables:
!-----------------------------------------------------------------------------
!LastSubreachInStreamSegment = The ending subreach within a stream segment (between gaging stations).
!BeginningSubreachInStreamSegment = The beginning subreach in a stream segment (between gaging stations).
!*****************************************************************************
use CommonVariables, &
     only : NumberOfFountainCreekDiversionDitches,NumberOfFountainCreekSegments,NumberOfFountainCreekSubreaches, &
     NumberOfMonumentCreekDiversionDitches,NumberOfMonumentCreekSegments,NumberOfMonumentCreekSegments, &
     NumberOfMonumentCreekSubreaches,SumOfFryArkDiversionsForCurrentDay,SumOfNonFryArkDiversionsForCurrentDay, &
     SegmentSumNativeFlowDiversionsAlongMonumentCreek,MonumentCreekGagingStationNode,FountainCreekGagingStationNode, &
     SubreachSumNativeFlowDiversionsAlongMonumentCreek,SegmentSumNativeFlowDiversionsAlongFountainCreek, &
     SegmentSumFryArkDiversionsAlongFountainCreek,SegmentSumExchangeDiversionsAlongFountainCreek, &
     SegmentTransitLossFromFromFountainCreekDiversion,SegmentSumReusedDiversionsAlongFountainCreek, &
     SubreachSumNativeFlowDiversionsAlongFountainCreek,MonumentCreekDiversionNode,NativeFlowDiversionAlongMonumentCreek, &
     SubreachSumFryArkDiversionsAlongFountainCreek,SubreachSumExchangeDiversionsAlongFountainCreek, &
     SubreachSumReusedDiversionsAlongFountainCreek,SubreachTransitLossFromFountainCreekDiversion, &
     FountainCreekDiversionNode,NativeFlowDiversionAlongFountainCreek,FryArkFlowDiversionAlongFountainCreek, &
     ExchangeDiversion1AlongFountainCreek,SubreachTransitLossFromFountainCreekDiversion,TransitLossSavingsDiversion, &
     CSRRFDiversion2AlongFountainCreek,SubreachTransitLossFromFountainCreekDiversion, iostatus
implicit none

integer :: BeginningSubreachInStreamSegment,LastSubreachInStreamSegment,CurrentSegment,CurrentSubreach,CurrentDitch

!Sum diversions for Monument Creek.
LoopThroughMonumentCreekSegments: do CurrentSegment=1,NumberOfMonumentCreekSegments
     !Set initial segment sums to zero.
     SegmentSumNativeFlowDiversionsAlongMonumentCreek(CurrentSegment)=0.0
     !Set variables for starting and ending subreach numbers in current segment.
     CheckForFirstMonumentCreekSegment: if (CurrentSegment == 1) then
          BeginningSubreachInStreamSegment=1
          LastSubreachInStreamSegment=MonumentCreekGagingStationNode(CurrentSegment+1)-1
     else
          BeginningSubreachInStreamSegment=LastSubreachInStreamSegment+1
          LastSubreachInStreamSegment=MonumentCreekGagingStationNode(CurrentSegment+1)-1
     end if CheckForFirstMonumentCreekSegment
     !For each subreach in segment...
     LoopThroughMonumentCreekSubreaches: do CurrentSubreach=BeginningSubreachInStreamSegment,LastSubreachInStreamSegment
          !Set initial segment sums to zero...
          SubreachSumNativeFlowDiversionsAlongMonumentCreek(CurrentSubreach)=0.0
          !Then check for ditches in subreach and add to subreach sum...
          LoopThroughAndSumDiversionsInMonumentCreekSubreach: do CurrentDitch=1,NumberOfMonumentCreekDiversionDitches
               if (MonumentCreekDiversionNode(CurrentDitch) == CurrentSubreach) &
                    SubreachSumNativeFlowDiversionsAlongMonumentCreek(CurrentSubreach)= &
                    SubreachSumNativeFlowDiversionsAlongMonumentCreek(CurrentSubreach) + &
                    NativeFlowDiversionAlongMonumentCreek(CurrentDitch)
          end do LoopThroughAndSumDiversionsInMonumentCreekSubreach
          !Then add each subreach sum to segment sum.
          SegmentSumNativeFlowDiversionsAlongMonumentCreek(CurrentSegment) = &  
               SegmentSumNativeFlowDiversionsAlongMonumentCreek(CurrentSegment)+ &
               SubreachSumNativeFlowDiversionsAlongMonumentCreek(CurrentSubreach)
     end do LoopThroughMonumentCreekSubreaches
end do LoopThroughMonumentCreekSegments

!Sum diversions for Fountain Creek.
SumOfFryArkDiversionsForCurrentDay=0.0
SumOfNonFryArkDiversionsForCurrentDay=0.0
LoopThroughFountainCreekSegments: do CurrentSegment=1,NumberOfFountainCreekSegments
     !Set initial segment sums to zero.
     SegmentSumNativeFlowDiversionsAlongFountainCreek(CurrentSegment)=0.0
     SegmentSumFryArkDiversionsAlongFountainCreek(CurrentSegment)=0.0
     SegmentSumExchangeDiversionsAlongFountainCreek(CurrentSegment)=0.0
     SegmentTransitLossFromFromFountainCreekDiversion(CurrentSegment)=0.0
     SegmentSumReusedDiversionsAlongFountainCreek(CurrentSegment)=0.0
     !Set variables for starting and ending subreach numbers in current segment.
     CheckForFirstFountainCreekSegment: if (CurrentSegment == 1) then
          BeginningSubreachInStreamSegment=1
          LastSubreachInStreamSegment=FountainCreekGagingStationNode(CurrentSegment+1)-1-NumberOfMonumentCreekSubreaches
     else if (CurrentSegment > 1 .and. CurrentSegment < NumberOfFountainCreekSegments) then
          BeginningSubreachInStreamSegment=LastSubreachInStreamSegment+1
          LastSubreachInStreamSegment=FountainCreekGagingStationNode(CurrentSegment+1)-1-NumberOfMonumentCreekSubreaches
     else if (CurrentSegment == NumberOfFountainCreekSegments) then
          BeginningSubreachInStreamSegment=LastSubreachInStreamSegment+1
          LastSubreachInStreamSegment=BeginningSubreachInStreamSegment
     end if CheckForFirstFountainCreekSegment
     !For each subreach in segment...
     LoopThroughFountainCreekSubreaches: do CurrentSubreach=BeginningSubreachInStreamSegment,LastSubreachInStreamSegment
          !Set initial subreach sums to zero...
          SubreachSumNativeFlowDiversionsAlongFountainCreek(CurrentSubreach)=0.0
          SubreachSumFryArkDiversionsAlongFountainCreek(CurrentSubreach)=0.0
          SubreachSumExchangeDiversionsAlongFountainCreek(CurrentSubreach)=0.0
          SubreachSumReusedDiversionsAlongFountainCreek(CurrentSubreach)=0.0
          SubreachTransitLossFromFountainCreekDiversion(CurrentSubreach)=0.0
          !Then check for ditches in subreach and add to subreach sum...
          LoopThroughAndSumDiversionsInFountainCreekSubreach: do CurrentDitch=1,NumberOfFountainCreekDiversionDitches
               CheckForFountainCreekDitch: if (FountainCreekDiversionNode(CurrentDitch) == &
                    CurrentSubreach+NumberOfMonumentCreekSubreaches) then
                    SubreachSumNativeFlowDiversionsAlongFountainCreek(CurrentSubreach)= &
                         SubreachSumNativeFlowDiversionsAlongFountainCreek(CurrentSubreach) + &
                         NativeFlowDiversionAlongFountainCreek(CurrentDitch)
                    SubreachSumFryArkDiversionsAlongFountainCreek(CurrentSubreach) = &
                         SubreachSumFryArkDiversionsAlongFountainCreek(CurrentSubreach)+ &
                         FryArkFlowDiversionAlongFountainCreek(CurrentDitch)
                    SubreachSumExchangeDiversionsAlongFountainCreek(CurrentSubreach) = &
                         SubreachSumExchangeDiversionsAlongFountainCreek(CurrentSubreach)+ &
                         ExchangeDiversion1AlongFountainCreek(CurrentDitch)
                    SubreachTransitLossFromFountainCreekDiversion(CurrentSubreach) = &
                         SubreachTransitLossFromFountainCreekDiversion(CurrentSubreach)+ &
                         TransitLossSavingsDiversion(CurrentDitch)
                    !Set value to zero--needed for batch (continuous) computation method (G. Kuhn, 10/05/2011).
                    TransitLossSavingsDiversion(CurrentDitch)=0.0
                    SubreachSumReusedDiversionsAlongFountainCreek(CurrentSubreach) = &
                         SubreachSumReusedDiversionsAlongFountainCreek(CurrentSubreach)+ &
                         CSRRFDiversion2AlongFountainCreek(CurrentDitch)
                end if CheckForFountainCreekDitch
           end do LoopThroughAndSumDiversionsInFountainCreekSubreach
           !Then add each subreach sum to segment sum.
           SegmentSumNativeFlowDiversionsAlongFountainCreek(CurrentSegment) = &
               SegmentSumNativeFlowDiversionsAlongFountainCreek(CurrentSegment)+ &
               SubreachSumNativeFlowDiversionsAlongFountainCreek(CurrentSubreach)
           SegmentSumFryArkDiversionsAlongFountainCreek(CurrentSegment) = &
               SegmentSumFryArkDiversionsAlongFountainCreek(CurrentSegment)+ &
               SubreachSumFryArkDiversionsAlongFountainCreek(CurrentSubreach)
           SegmentSumExchangeDiversionsAlongFountainCreek(CurrentSegment) = &
               SegmentSumExchangeDiversionsAlongFountainCreek(CurrentSegment)+ &
                SubreachSumExchangeDiversionsAlongFountainCreek(CurrentSubreach)
           SegmentTransitLossFromFromFountainCreekDiversion(CurrentSegment) = &
               SegmentTransitLossFromFromFountainCreekDiversion(CurrentSegment)+ &
               SubreachTransitLossFromFountainCreekDiversion(CurrentSubreach)
           SegmentSumReusedDiversionsAlongFountainCreek(CurrentSegment) = &
               SegmentSumReusedDiversionsAlongFountainCreek(CurrentSegment) + &
               SubreachSumReusedDiversionsAlongFountainCreek(CurrentSubreach)
           !Also sum all TM diversions for each reusable return flow for output report.
           SumOfFryArkDiversionsForCurrentDay = SumOfFryArkDiversionsForCurrentDay + &
               SubreachSumFryArkDiversionsAlongFountainCreek(CurrentSubreach)
           SumOfNonFryArkDiversionsForCurrentDay=SumOfNonFryArkDiversionsForCurrentDay + &
               SubreachSumExchangeDiversionsAlongFountainCreek(CurrentSubreach) + &
               SubreachSumReusedDiversionsAlongFountainCreek(CurrentSubreach)
     end do LoopThroughFountainCreekSubreaches
end do LoopThroughFountainCreekSegments

return
end

!*****************************************************************************
Subroutine ReadAndWriteBackupFiles (TempDate,InputFileUnit1,InputFileUnit2,InputFileUnit3,InputFileUnit4, &
     OutputFileUnit1,OutputFileUnit2,OutputFileUnit3,OutputFileUnit4,InputErrorFlag)
!*****************************************************************************
!Subroutine reads and writes appropriate files to maintain 'back1.fil' and 'back2.fil'.  Subroutine also reads and writes
!these files for recomputing transit loss for last day of run or the day prior to the last run date.

!Definition of local variables:
!-----------------------------------------------------------------------------
!DummyDate,TempDate = variables for dummy/temporary date value.
!RecoveryFileHeader = character string at the beginning of "both_recov.fil" used for identification purposes.
!*****************************************************************************
use CommonVariables, only : ACCTSFileUnit, iostatus
implicit none

integer, intent(in) :: InputFileUnit1,InputFileUnit2,InputFileUnit3,InputFileUnit4, &
     OutputFileUnit1,OutputFileUnit2,OutputFileUnit3,OutputFileUnit4
integer, intent(out) :: InputErrorFlag 
character :: DummyDate*8,TempDate*8 

!data SubreachBankStorageRecoveryPeriodLengthInDays /4,3,1,1,2,2,1,1,1,2,2,1,12,8,28,18*60/

InputErrorFlag=0
read (InputFileUnit1,1000,iostat=iostatus) DummyDate
if (iostatus /= 0) then
     InputErrorFlag = 1
     call ReportInputError()
     return
end if
!Read and write gaging station, return flow, and discharge data.
call ReadWebinFileData (InputFileUnit1,OutputFileUnit1,TempDate,InputErrorFlag)
if (InputErrorFlag > 0) return

!Read/write monthly and annual summaries (ACCUMFileUnit.) of reusable return flow data.
call ReadAndWriteMonthlyAnnualRRFSums (InputFileUnit2,OutputFileUnit2,InputErrorFlag)
if (InputErrorFlag > 0) return

!Read and write transmountain diversion accounts data.
call UpdateAccountsFile (InputFileUnit3,OutputFileUnit3,InputErrorFlag)
if (InputErrorFlag > 0) return

!Write recovery data for "BankStorageRecoveryFileUnit."
call WriteRecoveryData (InputFileUnit4,OutputFileUnit4,InputErrorFlag)
if (InputErrorFlag > 0) return

rewind (InputFileUnit1)
rewind (InputFileUnit2)
rewind (InputFileUnit3)
rewind (InputFileUnit4)
rewind (OutputFileUnit1)
rewind (OutputFileUnit2)
rewind (OutputFileUnit3)
rewind (OutputFileUnit4)
rewind (ACCTSFileUnit)

return
1000 format (a8)
end

!*****************************************************************************
subroutine ReadWebinFileData (ReadFileUnit,WriteFileUnit,DummyDate,InputErrorFlag)
!*****************************************************************************
!Subroutine reads and writes previous, current, or temporary input data from the main program input file (Webin.fil). Data are read/written
!as data strings rather than in specific formats.

!Definition of local variables:
!-----------------------------------------------------------------------------
!ReadFileUnit = local unit number for file from which data is read.
!WriteFileUnit = local unit number for file to which data is written.
!DummyDate = variable for dummy/temporary date value.
!ScratchFileData1--ScratchFileData6 = character strings used to read data in scratch file and write it to one of saved program i/o files.
!EndDataTypeFlag = character string to identify end of each data type.
!*****************************************************************************
use CommonVariables,only : TextOutputFileUnit, iostatus
implicit none

integer, intent(out) :: InputErrorFlag
integer, intent(in) :: ReadFileUnit, WriteFileUnit
integer :: CurrentFlow
character, intent(in) :: DummyDate*8
character :: ScratchFileData1*66,ScratchFileData2*72,ScratchFileData3*79,ScratchFileData4*75, &
     ScratchFileData5*42,ScratchFileData6*66,EndDataTypeFlag*2

InputErrorFlag=0
write (WriteFileUnit,1000) DummyDate

!Read and write station data.
LoopThroughAndReadWriteGagedMainstemFlows: do CurrentFlow=1,100
     read (ReadFileUnit,1010,iostat=iostatus) ScratchFileData1
      ! read error
      if (iostatus /= 0 ) then
           write (TextOutputFileUnit,2000) 'In subroutine ReadWebinFileData--Read error, gaging station data.'
           InputErrorFlag = 1
          return
     end if 
     write (WriteFileUnit,1010) ScratchFileData1
     EndDataTypeFlag=ScratchFileData1(1:2)
     if (EndDataTypeFlag == 'EN') exit
end do LoopThroughAndReadWriteGagedMainstemFlows

!Read and write tributary data.
LoopThroughAndReadWriteGagedTributaryFlows: do CurrentFlow=1,100
     read (ReadFileUnit,1010,iostat=iostatus) ScratchFileData1
      ! read error
      if (iostatus /= 0 ) then
           write (TextOutputFileUnit,2000) 'In subroutine ReadWebinFileData--Read error, tributary data.'
           InputErrorFlag = 1
          return
     end if 
     write (WriteFileUnit,1010) ScratchFileData1
     EndDataTypeFlag=ScratchFileData1(1:2)
     if (EndDataTypeFlag == 'EN') exit
end do LoopThroughAndReadWriteGagedTributaryFlows

!Read and write native return flow data.
LoopThroughAndReadWriteNativeReturnFlows: do CurrentFlow=1,100
      read (ReadFileUnit,1020,iostat=iostatus) ScratchFileData2
      ! read error
      if (iostatus /= 0 ) then
           write (TextOutputFileUnit,2000) 'In subroutine ReadWebinFileData--Read error, native return flow data.'
           InputErrorFlag = 1
          return
     end if 
      write (WriteFileUnit,1020) ScratchFileData2
      EndDataTypeFlag=ScratchFileData2(1:2)
      if (EndDataTypeFlag == 'EN') exit
end do LoopThroughAndReadWriteNativeReturnFlows

!Read and write reusable return flow data.
LoopThroughAndReadWriteReusableReturnFlows: do CurrentFlow=1,500
     read (ReadFileUnit,1030,iostat=iostatus) ScratchFileData3
      ! read error
      if (iostatus /= 0 ) then
           write (TextOutputFileUnit,2000) 'In subroutine ReadWebinFileData--Read error, reusable return flow data.'
           InputErrorFlag = 1
          return
     end if 
     write (WriteFileUnit,1030) ScratchFileData3
     EndDataTypeFlag=ScratchFileData3(1:2)
     if (EndDataTypeFlag == 'EN') exit
end do LoopThroughAndReadWriteReusableReturnFlows

!Read and write data for diversion (off-stream) of reusable return flows.
LoopThroughAndReadWriteReusableReturnFlowDiversions: do CurrentFlow=1,100
     read (ReadFileUnit,1040,iostat=iostatus) ScratchFileData4
      ! read error
      if (iostatus /= 0 ) then
           write (TextOutputFileUnit,2000) 'In subroutine ReadWebinFileData--Read error, reusable diversions data.'
           InputErrorFlag = 1
          return
     end if 
     write (WriteFileUnit,1040) ScratchFileData4
     EndDataTypeFlag=ScratchFileData4(1:2)
     if (EndDataTypeFlag == 'EN') exit
end do LoopThroughAndReadWriteReusableReturnFlowDiversions

!Read and write Monument Creek native streamflow diversion data.
LoopThroughAndReadWriteMonumentCreekNativeReturnFlowDiversions: do CurrentFlow=1,100
     read (ReadFileUnit,1050,iostat=iostatus) ScratchFileData5
      ! read error
      if (iostatus /= 0 ) then
           write (TextOutputFileUnit,2000) 'In subroutine ReadWebinFileData--Read error, Monument native diversions.'
           InputErrorFlag = 1
          return
     end if 
     write (WriteFileUnit,1050) ScratchFileData5
     EndDataTypeFlag=ScratchFileData5(1:2)
     if (EndDataTypeFlag == 'EN') exit
end do LoopThroughAndReadWriteMonumentCreekNativeReturnFlowDiversions

!Read and write Fountain Creek native streamflow diversion data. Includes diversions of reusable water sold to other users.
LoopThroughAndReadWriteFountainCreekNativeReturnFlowDiversions: do CurrentFlow=1,100
     read (ReadFileUnit,1010,iostat=iostatus) ScratchFileData6
      ! read error
      if (iostatus /= 0 ) then
           write (TextOutputFileUnit,2000) 'In subroutine ReadWebinFileData--Read error, Fountain native diversions.'
           InputErrorFlag = 1
          return
     end if 
     write (WriteFileUnit,1010) ScratchFileData6
     EndDataTypeFlag=ScratchFileData6(1:2)
     if (EndDataTypeFlag == 'EN') exit
end do LoopThroughAndReadWriteFountainCreekNativeReturnFlowDiversions

!Read and write any remarks for output.
LoopThroughAndReadWriteRemarks: do CurrentFlow=1,10
     read (ReadFileUnit,1010,iostat=iostatus) ScratchFileData1
      ! read error
      if (iostatus /= 0 ) then
           write (TextOutputFileUnit,2000) 'In subroutine ReadWebinFileData--Read error, Remarks for output.'
           InputErrorFlag = 1
          return
     end if 
     
     if (ScratchFileData1 == '') cycle
     EndDataTypeFlag=ScratchFileData1(1:2)
     if (EndDataTypeFlag == '  ') cycle
     write (WriteFileUnit,1010) ScratchFileData1
     if (EndDataTypeFlag == 'EN') return
end do LoopThroughAndReadWriteRemarks

return

1000 format (a8)
1010 format (a66)
1020 format (a72)
1030 format (a79)
1040 format (a75)
1050 format (a42)
2000 format (a100)

end

!*****************************************************************************
subroutine ReadAndWriteMonthlyAnnualRRFSums (ReadFileUnit,WriteFileUnit,InputErrorFlag)
!*****************************************************************************
!Subroutine reads and writes previous, current, or temporary data values for monthly and annual reusable return flow volumes (reusable_sum.fil).
!Data are read/written as data strings rather than in specific formats.

!Definition of local variables:
!-----------------------------------------------------------------------------
!ReadFileUnit = local unit number for file from which data is read.
!WriteFileUnit = local unit number for file to which data is written.
!ScratchFileData1--ScratchFileData6 = character strings used to read data in scratch file and write it to one of saved program i/o files.
!EndDataTypeFlag = character string to identify end of each data type.
!*****************************************************************************

use CommonVariables, only : TextOutputFileUnit,iostatus                                              
implicit none

integer, intent(out) :: InputErrorFlag
integer, intent(in) :: ReadFileUnit,WriteFileUnit
integer :: CurrentReturnFlowRelease                                      
      
character :: ScratchFileData1*95,EndDataTypeFlag*2

InputErrorFlag=0

!Read and write monthly & annual accumulated reusable return flow release values.
LoopThroughAndReadWriteReusableReturnFlowReleases: do CurrentReturnFlowRelease=1,500
     read (ReadFileUnit,1000,iostat=iostatus) ScratchFileData1
      ! good
      if (iostatus == 0 ) then
           write (WriteFileUnit,1000) ScratchFileData1
           EndDataTypeFlag=ScratchFileData1(1:2)
           if (EndDataTypeFlag == 'EN') return
      ! read error
      else if (iostatus > 0 ) then
           write (TextOutputFileUnit,1020) &
               'In subroutine ReadAndWriteMonthlyAnnualRRFSums--read error, monthly/annual summary.'
           InputErrorFlag = 1
          return
      ! EOF
      else if (iostatus < 0) then
           return
      else
         InputErrorFlag = 1
         return
     end if 
  
end do LoopThroughAndReadWriteReusableReturnFlowReleases

999 return

1000 format (a95)
1020 format (a100)
end

!*****************************************************************************
subroutine UpdateAccountsFile (ReadFileUnit,WriteFileUnit,InputErrorFlag)
!*****************************************************************************
!Subroutine reads previous, current, or temporary data values for diversion accounts (acct.fil).

!Definition of local variables:
!-----------------------------------------------------------------------------
!ReadFileUnit = local unit number for file from which data is read.
!WriteFileUnit = local unit number for file to which data is written.
!NumberOfDitches = number of ditches and/or accounts.
!CharacterLineOfDataInACCTSFile = character string for reading a line of data.
!*****************************************************************************
use CommonVariables, &
     only : ACCTSFileUnit,TextOutputFileUnit,AmountOfCSFryArkWaterAvailable,AmountOfCSFryArkWaterUsed, &
     AmountOfCSExchangeWaterAvailable,AmountOfCSExchangeWaterUsed,AmountOfCSWaterAvailable,AmountOfCSWaterUsed,iostatus
implicit none

integer, intent(in) :: ReadFileUnit,WriteFileUnit
integer, intent(out) :: InputErrorFlag
integer :: NumberOfDitches,CurrentDiversion


character :: CharacterLineOfDataInACCTSFile*32

InputErrorFlag=0

!Changed 08/15/09 because of new Fountain Creek diversions. Added loop to read "ACCTSFileUnit" in order to get number of ditches. Variable (NumberOfDitches) is not
!set when using option 4. G. Kuhn, 12/02/09.

NumberOfDitches=0

LoopThroughAndCountNumberOfDiversions: do CurrentDiversion=1,100    
     read (ACCTSFileUnit,1000,iostat=iostatus) CharacterLineOfDataInACCTSFile
     ! test if good or EOF, increment ditch count
      
      if (iostatus == 0 ) then        ! good
           NumberOfDitches=NumberOfDitches+1
      else if (iostatus < 0) then      ! EOF
           exit        
      else                             ! read error
           write (TextOutputFileUnit,1020) 'In subroutine UpdateAccountsFile--error reading accounts data.'
           InputErrorFlag = 1
          return     
     end if      
end do LoopThroughAndCountNumberOfDiversions

!#Rewind to top of file for next read.
rewind (ACCTSFileUnit)

!Read new or backup "have" and "used" account data from appropriate file.
LoopThroughAndReadUsedAndHaveData: do CurrentDiversion=1,NumberOfDitches
 read (ReadFileUnit,1010,iostat=iostatus) &
               AmountOfCSFryArkWaterAvailable(CurrentDiversion), &
               AmountOfCSFryArkWaterUsed(CurrentDiversion),  &
               AmountOfCSExchangeWaterAvailable(CurrentDiversion), &
               AmountOfCSExchangeWaterUsed(CurrentDiversion), &
               AmountOfCSWaterAvailable(CurrentDiversion),AmountOfCSWaterUsed(CurrentDiversion)
          
           if (iostatus /= 0) then       !  test Bad data line and bail
              write (TextOutputFileUnit,1020) 'In subroutine UpdateAccountsFile--error parsing line.'
                InputErrorFlag = 1
             return
           end if
end do  LoopThroughAndReadUsedAndHaveData

!Write new or backup "have" and "used" water purchases to appropriate program accounts file.
call WriteDiversionAccountData (WriteFileUnit,NumberOfDitches,InputErrorFlag)

return

1000 format (a32)
1010 format (32x,6(1x,f6.1))
1020 format (a100)

end

!*****************************************************************************
subroutine WriteDiversionAccountData (WriteFileUnit,NumberOfDitches,InputErrorFlag)
!*****************************************************************************
!Subroutine writes previous, current, or temporary data values for diversion accounts (acct.fil) read in previous subroutine (UpdateAccountsFile).

!Definition of local variables:
!-----------------------------------------------------------------------------
!WriteFileUnit = local unit number for file to which data is written.
!NumberOfDitches = number of ditches and/or accounts.
!CharacterLineOfDataInACCTSFile = character string array to VariableHeldForScratchFileInputOutput the names of each ditch account (same as ditch name).
!*****************************************************************************
use CommonVariables, &
     only : AmountOfCSExchangeWaterUsed,AmountOfCSExchangeWaterAvailable,AmountOfCSWaterAvailable, &
     AmountOfCSWaterUsed,AmountOfCSFryArkWaterUsed,AmountOfCSFryArkWaterAvailable,ACCTSFileUnit,TextOutputFileUnit,iostatus
implicit none
integer, intent(in) :: WriteFileUnit,NumberOfDitches
integer, intent(out) :: InputErrorFlag
integer :: CurrentDitch

character :: CharacterLineOfDataInACCTSFile(50)*32

InputErrorFlag=0
rewind (ACCTSFileUnit)

!Read account names from accounts file used in daily program runs.
LoopThroughAndReadAccountNames: do CurrentDitch=1,NumberOfDitches
     read (ACCTSFileUnit,1000,iostat=iostatus) CharacterLineOfDataInACCTSFile(CurrentDitch)
     if (iostatus /= 0) then       !  test Bad data line and bail
          write (TextOutputFileUnit,1020) 'In subroutine WriteDiversionAccountData--read error in "acct.fil".'
           InputErrorFlag = 1
          return
     end if  
end do LoopThroughAndReadAccountNames

!Write new data, from program run, backup files, or accts_in.fil to accounts file used in program or new backup file.
rewind (ACCTSFileUnit)

LoopThroughAndWriteToAccountsFile: do CurrentDitch=1,NumberOfDitches
     write (WriteFileUnit,1010) &
          CharacterLineOfDataInACCTSFile(CurrentDitch), AmountOfCSFryArkWaterAvailable(CurrentDitch), &
          AmountOfCSFryArkWaterUsed(CurrentDitch), AmountOfCSExchangeWaterAvailable(CurrentDitch), &
          AmountOfCSExchangeWaterUsed(CurrentDitch), &
          AmountOfCSWaterAvailable(CurrentDitch),AmountOfCSWaterUsed(CurrentDitch)       
end do LoopThroughAndWriteToAccountsFile

return

1000 format (a32)
1010 format (a32,6(';',f6.1))
1020 format (a100)
end

!*****************************************************************************
subroutine WriteRecoveryData (ReadFileUnit,WriteFileUnit,InputErrorFlag)
!*****************************************************************************
!Subroutine to write recovery files data and retain a consistent readable format--5 values per line, e format.

!Definition of local variables:
!-----------------------------------------------------------------------------
!ReadFileUnit = local unit number for file from which data is read.
!WriteFileUnit = local unit number for file to which data is written.
!VariableHeldForScratchFileInputOutput = same as defined in "q_gages.cmn."
!RecoveryFileHeader = character string at the beginning of "both_recov.fil" used for identification purposes.
!*****************************************************************************
use CommonVariables, &
     only : NumberOfFountainCreekSubreaches,NumberOfMonumentCreekSubreaches,VariableHeldForScratchFileInputOutput, &
     TextOutputFileUnit,NumberOfReusableReturnFlowsEntered,SubreachBankStorageRecoveryPeriodLengthInDays, iostatus

implicit none
integer, intent(in) :: ReadFileUnit,WriteFileUnit
integer, intent(out) :: InputErrorFlag
integer :: NumberOfOutputValues,NumberOfSubreaches,CurrentSubreach,CurrentRRF,CurrentOutputValue,BeginningValueOnLastLine, &
     NumberOfOutputLines,BeginningValue,EndValue,CurrentLine
     
      
character :: RecoveryFileHeader*80

InputErrorFlag=0

read (ReadFileUnit,1000,iostat=iostatus) RecoveryFileHeader

if (iostatus /= 0) then       !  test Bad data line and bail
      write (TextOutputFileUnit,1020) 'In subroutine WriteRecoveryData--Header read error for "both_recov.fil".'
      InputErrorFlag = 1
      return
end if    

write (WriteFileUnit,1000) RecoveryFileHeader

NumberOfSubreaches=NumberOfMonumentCreekSubreaches+NumberOfFountainCreekSubreaches
LoopThroughPredictedVariablesAtEachRRFNode: do CurrentSubreach=1,NumberOfSubreaches
    read (ReadFileUnit,1000,iostat=iostatus) RecoveryFileHeader
     if (iostatus /= 0) Go to 910       !  test Bad data line and bail
     write (WriteFileUnit,1000) RecoveryFileHeader
     NumberOfOutputValues=SubreachBankStorageRecoveryPeriodLengthInDays(CurrentSubreach)+1
     LoopThroughPredictedVariablesAtCurrentRRFNode: do CurrentRRF=1,NumberOfReusableReturnFlowsEntered
          read (ReadFileUnit,1000,iostat=iostatus) RecoveryFileHeader
            if (iostatus /= 0) Go to 910       !  test Bad data line and bail
          write (WriteFileUnit,1000) RecoveryFileHeader
          read (ReadFileUnit,*,iostat=iostatus) &
               (VariableHeldForScratchFileInputOutput(CurrentOutputValue), CurrentOutputValue=1,NumberOfOutputValues)
            if (iostatus /= 0) Go to 910       !  test Bad data line and bail
          CheckForMoreThan6ValuesAtCurrentRRFNode: if (NumberOfOutputValues >= 6) then
               BeginningValueOnLastLine=NumberOfOutputValues-mod(NumberOfOutputValues,5)+1
               !Divide the  number of values by 5 to get the total number of lines that will be written, 5 values per line.
               NumberOfOutputLines=NumberOfOutputValues/5
               BeginningValue=1
               EndValue=5
               LoopWriteToOutputFile5ValuesPerLine: do CurrentLine=1,NumberOfOutputLines
                    write (WriteFileUnit,1010) &
                         (VariableHeldForScratchFileInputOutput(CurrentOutputValue),CurrentOutputValue=BeginningValue,EndValue)
                    BeginningValue=BeginningValue+5
                    EndValue=EndValue+5
               end do LoopWriteToOutputFile5ValuesPerLine
               write (WriteFileUnit,1010) &
                    (VariableHeldForScratchFileInputOutput(CurrentOutputValue), &
                    CurrentOutputValue=BeginningValueOnLastLine,NumberOfOutputValues)
          else
               write (WriteFileUnit,1010) &
                    (VariableHeldForScratchFileInputOutput(CurrentOutputValue),CurrentOutputValue=1,NumberOfOutputValues)
         end if CheckForMoreThan6ValuesAtCurrentRRFNode
     end do LoopThroughPredictedVariablesAtCurrentRRFNode
     read (ReadFileUnit,*,iostat=iostatus) (VariableHeldForScratchFileInputOutput(CurrentOutputValue), CurrentOutputValue=1,2)
      if (iostatus /= 0) Go to 910       !  test Bad data line and bail
     write (WriteFileUnit,*) (VariableHeldForScratchFileInputOutput(CurrentOutputValue), CurrentOutputValue=1,2)
end do LoopThroughPredictedVariablesAtEachRRFNode

return

910 write (TextOutputFileUnit,1020) 'In subroutine WriteRecoveryData--read error for "both_recov.fil".'
InputErrorFlag=1

return

1000 format (a80)
1010 format (5(e15.6))
1020 format (a100,/)

end

!***************************************************************************
subroutine ReadBankStorageLossLookupTable (InputErrorFlag)
!***************************************************************************
!Subroutine reads in arrays of subreach values (look-up tables) of bank storage loss for  Monument and Fountain Creeks to be used in
!subroutines for transit loss calculations.

!Definition of local variables:
!-----------------------------------------------------------------------------
!BankStorageLookupTableFile = name of file with bank-storage look-up tables.
!***************************************************************************
use CommonVariables, &
     only :  FileWithIOError,InputFilePathname,NumberOfMonumentCreekSubreaches,NumberOfFountainCreekSubreaches, &
     LowerRangeOfIntervalsForRRFBankStorageLossLookupTableEntry,UpperRangeOfIntervalsForRRFBankStorageLossLookupTableEntry, &
     RRFBankStorageLossLookupTableForBothCreeks,RRFBankStorageLossLookupTableMonumentCreekLowRRF,TextOutputFileUnit, iostatus
implicit none
integer, intent(out) :: InputErrorFlag
integer :: BankStorageLookupTableFileUnit,BankStorageLookupTableForLowMonumentCreekFlowsFileUnit, &
     InputFilePathnameLength,InputFilenameLength,FirstRRFEntryInBankStorageLossTable,CurrentSubreach, &
     LastRRFEntryInBankStorageLossTable,CurrentRRFTableEntry,CurrentNativeFlowTableEntry



!#The name of the bank storage loss file can be up to 200 characters long.      
character :: BankStorageLookupTableFile*200

InputErrorFlag=0
! BankStorageLookupTableFileUnit=60

!Open original bank storage loss file.  
InputFilePathnameLength = len(trim(InputFilePathname))
BankStorageLookupTableFile = 'both.bsfile.dat'
InputFilenameLength = len(trim(BankStorageLookupTableFile)) 
BankStorageLookupTableFile = InputFilePathname(:InputFilePathnameLength)//BankStorageLookupTableFile(:InputFilenameLength)
FileWithIOError=BankStorageLookupTableFile

!#If problems with opening the both.bsfile.dat file, transfer control to the write statement that reports this to the user.
open (unit=BankStorageLookupTableFileUnit,file=BankStorageLookupTableFile,status='old',iostat=iostatus)

if (iostatus /= 0) then       !  Trap error on file open
      write (TextOutputFileUnit,1010) 'In ReadBankStorageLossLookupTable--unable to open "both.bsfile.dat" file.'
      call ReportInputError()
      InputErrorFlag = 1
      return
end if    

FirstRRFEntryInBankStorageLossTable=1

!#Loop through all Monument Creek and Fountain Creek subreaches.
LoopThroughAllSubreaches: do CurrentSubreach=1,NumberOfMonumentCreekSubreaches+NumberOfFountainCreekSubreaches
    !#There are 200 discrete return flows in the bank-storage loss table for each subreach.
     LastRRFEntryInBankStorageLossTable=FirstRRFEntryInBankStorageLossTable+199
     !Read and skip the heading lines.
     read (BankStorageLookupTableFileUnit,*,iostat=iostatus)
      
      !# On EOF exit loop, Error bail, good keep reading
      if (iostatus < 0) then     !EOF
          exit
     else if (iostatus > 0) then    !  Trap error on file read
           write (TextOutputFileUnit,1010) 'In ReadBankStorageLossLookupTable--unable to read "both.bsfile.dat" file.'
           call ReportInputError()
           InputErrorFlag = 1
           return
     end if    

     !Read data lines.
     !#For the current subreach, read all 16 discrete native-flow values (0,1,2,5,10,20,30,40,50,75,100,200,300,500,700 and 1000 cfs) 
     !for all 100 discrete return-flow values.
     LoopThroughAllDiscreteReturnFlowsInCurrentSubreach: do CurrentRRFTableEntry = FirstRRFEntryInBankStorageLossTable, &
               LastRRFEntryInBankStorageLossTable
          !#If problems reading from the both.bsfile.dat file, report it to the user.
          read (BankStorageLookupTableFileUnit,1000,iostat=iostatus) &
               (RRFBankStorageLossLookupTableForBothCreeks(CurrentRRFTableEntry,CurrentNativeFlowTableEntry), &
               CurrentNativeFlowTableEntry=1,16)
            
          if (iostatus /= 0) then       !  Trap error on file read
           write (TextOutputFileUnit,1010) 'In ReadBankStorageLossLookupTable--"both.bsfile.dat" file is incomplete.'
           call ReportInputError()
           InputErrorFlag = 1
           return
          end if  

     end do LoopThroughAllDiscreteReturnFlowsInCurrentSubreach
     !#Increment the discrete return-flow counter by 1 before reading more discrete return flows for the next subreach.
     FirstRRFEntryInBankStorageLossTable=LastRRFEntryInBankStorageLossTable+1
end do LoopThroughAllSubreaches

!#You've reached the end of the both.bsfile.dat file, so close it.
30 close (BankStorageLookupTableForLowMonumentCreekFlowsFileUnit)

!#Subtract 1 from the final value of the LoopThroughAllSubreaches counter to get the total number of subreaches read.
!#If not equal to the total number of subreaches, there's a problem with the both.bsfile.dat file. 
!# Warn user and return.
if (CurrentSubreach-1 /= NumberOfMonumentCreekSubreaches+NumberOfFountainCreekSubreaches) then
     write (TextOutputFileUnit,1010) 'In ReadBankStorageLossLookupTable--"both.bsfile.dat" file is incomplete.'
      call ReportInputError()
      InputErrorFlag = 1
     return
end if

!Open new bank storage loss file for small reusable return flows on Monument Creek.

!# Why is the file unit number specified locally? Should be defined along with all other unit numbers.
!  BankStorageLookupTableForLowMonumentCreekFlowsFileUnit=59

InputFilePathnameLength = len(trim(InputFilePathname))
BankStorageLookupTableFile = "Mon_low.bsfile.dat"
InputFilenameLength = len(trim(BankStorageLookupTableFile))
BankStorageLookupTableFile = InputFilePathname(:InputFilePathnameLength)//BankStorageLookupTableFile(:InputFilenameLength)
FileWithIOError=BankStorageLookupTableFile

!#If problems with opening the Mon_low.bsfile.dat file, transfer control to the write statement
!#that reports this to the user.
open (unit=BankStorageLookupTableForLowMonumentCreekFlowsFileUnit,file=BankStorageLookupTableFile, &
      status='old', iostat=iostatus)
!# Error on file open
if (iostatus /= 0) then
    write (TextOutputFileUnit,1010) 'In ReadBankStorageLossLookupTable--unable to open "Mon_low.bsfile.dat" file.'
     call ReportInputError()
     InputErrorFlag = 1
     return
end if

!Read data into the array.
!#Reset the initial counter.
FirstRRFEntryInBankStorageLossTable=1

!#Loop through all Monument Creek subreaches, where return flows tend to be smaller and you need greater resolution on looking 
!#up return flow values.
LoopThroughAlMonthOfLastReleaseDateumentCreekSubreaches: do CurrentSubreach=1,NumberOfMonumentCreekSubreaches
     !#There are 26 records in the lookup table for each subreach, so add 25 to 1.
     LastRRFEntryInBankStorageLossTable=FirstRRFEntryInBankStorageLossTable+25
     !Read and skip the heading lines.
     read (BankStorageLookupTableForLowMonumentCreekFlowsFileUnit,*,iostat=iostatus)
           
      !# On EOF exit loop, Error bail, good keep reading
      if (iostatus < 0) then     !EOF
          exit
     else if (iostatus > 0) then    !  Trap error on file read
           write (TextOutputFileUnit,1010) 'In ReadBankStorageLossLookupTable--unable to read "Mon_low.bsfile.dat" file.'
           call ReportInputError()
           InputErrorFlag = 1
           return
     end if    
     
     !Read data lines.
     LoopThroughAllDiscreteReturnFlowsInCurrentMonumentSubreach:do CurrentRRFTableEntry = &
               FirstRRFEntryInBankStorageLossTable,LastRRFEntryInBankStorageLossTable
          !#If problems reading from the mon_low.bsfile.dat file, report it to the user.
          read (BankStorageLookupTableForLowMonumentCreekFlowsFileUnit,1005,iostat=iostatus) &
               LowerRangeOfIntervalsForRRFBankStorageLossLookupTableEntry(CurrentRRFTableEntry), &
               UpperRangeOfIntervalsForRRFBankStorageLossLookupTableEntry(CurrentRRFTableEntry), &
               (RRFBankStorageLossLookupTableMonumentCreekLowRRF &
               (CurrentRRFTableEntry,CurrentNativeFlowTableEntry),CurrentNativeFlowTableEntry=1,16)
                      
            if (iostatus /= 0) then       !  Trap error on file read
                write (TextOutputFileUnit,1010) 'In ReadBankStorageLossLookupTable--"Mon_low.bsfile.dat" file is incomplete.'
                call ReportInputError()
                InputErrorFlag = 1
                return
            end if  
            
     end do LoopThroughAllDiscreteReturnFlowsInCurrentMonumentSubreach
     !#Increment the discrete return-flow counter by 1 before reading more discrete return flows for the next Monument Creek subreach.
     FirstRRFEntryInBankStorageLossTable=LastRRFEntryInBankStorageLossTable+1
end do LoopThroughAlMonthOfLastReleaseDateumentCreekSubreaches

!#You've reached the end of the mon_low.bsfile.dat file, so close it.
60 close (BankStorageLookupTableForLowMonumentCreekFlowsFileUnit)

!#Subtract 1 from the final value of the LoopThroughAlMonthOfLastReleaseDateumentSubreaches counter to get the total number of subreaches read.
!#If not equal to the total number of Monument subreaches, there's a problem with the Mon_low.bsfile.dat
!#so warn user and return.
if (CurrentSubreach-1 /= NumberOfMonumentCreekSubreaches) then
     write (TextOutputFileUnit,1010) 'In ReadBankStorageLossLookupTable--"Mon_low.bsfile.dat" file is incomplete.'
      call ReportInputError()
      InputErrorFlag = 1
     return
end if

999 return

1000 format (14x,16(f7.3))
1005 format (13x,2(f8.2),1x,16(f7.4))
1010 format (a100,/)

end

!*****************************************************************************
subroutine ReadBankStorageLossRecoveryTable (InputErrorFlag)
!*****************************************************************************
!Subroutine does a one-time read of the recovery file and places the data in an array for random access during transit loss computations. After all
!computations are completed, new bank storage data saved in "scratch1" file are written back to the recovery file. This change made from prior 
!versions in which recovery file was read sequentially, rewound, and read again with each iteration of computations. Change made 12/2011, G. Kuhn.

!Definition of local variables:
!-----------------------------------------------------------------------------
!RecoveryFileUnit = file number for recovery file.
!SubreachNumber = local temporary variable for the subreach number.
!*****************************************************************************
use CommonVariables, &
     only : SubreachBankStorageReleasedDuringRecovery,NumberOfFountainCreekSubreaches, &
     NumberOfMonumentCreekSubreaches,BankStorageRecoveryFileUnit,NumberOfReusableReturnFlowsEntered, &
     SubreachBankStorageRecoveryPeriodLengthInDays, SubreachChannelStorageGainAlongMonumentCreek, &
     SubreachChannelStorageGainAlongFountainCreek,SubreachChannelStorageGainAlongMonumentCreek, &
     SubreachChannelStorageLossAlongFountainCreek,SubreachChannelStorageLossAlongMonumentCreek, &
     FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions,BankStorageRecoveryFileUnit,TextOutputFileUnit,iostatus
implicit none
integer, intent(out) :: InputErrorFlag
integer :: TotalNumberOfSubreaches,CurrentSubreach,CurrentRRF,CurrentNativeFlow,LengthOfBankStorageRecoveryPeriod

      
character :: RecoveryFileHeader*80,SubreachNumber*12,InputOrOutputFilename*80

!Need to read recovery values by reach, then by reusable return flow entity, then by number of values in recovery period.

InputErrorFlag=0
rewind (BankStorageRecoveryFileUnit)
read (BankStorageRecoveryFileUnit,1000,iostat=iostatus) RecoveryFileHeader
!# Error on file open
if (iostatus /= 0) then
    write (TextOutputFileUnit,1020) &
     'Bank storage recovery file ("both_recov.fil") incomplete in subroutine ReadBankStorageLossRecoveryTable'
     call ReportInputError()
     InputErrorFlag = 1
     return
end if
TotalNumberOfSubreaches=NumberOfMonumentCreekSubreaches+NumberOfFountainCreekSubreaches

LoopThroughAndReadStreamSubreachNumber: do CurrentSubreach=1,TotalNumberOfSubreaches
     !Read subreach number as a dummy variable.
     read (BankStorageRecoveryFileUnit,1010, iostat=iostatus) SubreachNumber

      !# On  Error bail
      if (iostatus /= 0) then       !  Trap error on file read
           write (TextOutputFileUnit,1020) &
           'Bank storage recovery file ("both_recov.fil") incomplete in subroutine ReadBankStorageLossRecoveryTable.'
           call ReportInputError()
           InputErrorFlag = 1
           return
     end if    
     
     LengthOfBankStorageRecoveryPeriod=SubreachBankStorageRecoveryPeriodLengthInDays(CurrentSubreach)
     LoopThroughAndReadInputRRFReleases: do CurrentRRF=1,NumberOfReusableReturnFlowsEntered
          !Initialize each array element before reading in the data values.
          LoopAndZeroOutBankStorageOverRecoveryPeriod: do CurrentNativeFlow=1,61
               SubreachBankStorageReleasedDuringRecovery(CurrentSubreach,CurrentRRF,CurrentNativeFlow)=0.0
          end do LoopAndZeroOutBankStorageOverRecoveryPeriod
            
          !Read names of reusable return flows as a dummy variable 
          !(they were read into an array during data input in "subroutine ReadReturnFlowInput."
          read (BankStorageRecoveryFileUnit,1000, iostat=iostatus) InputOrOutputFilename
            if (iostatus /= 0) then      !  Trap error on file read
                     write (TextOutputFileUnit,1020) &
                     'Bank storage recovery file ("both_recov.fil") incomplete in subroutine ReadBankStorageLossRecoveryTable.'
                     InputErrorFlag = 1
                     return
            end if  
     
          !Read bank storage values for each return flow for length of recovery period.
          read (BankStorageRecoveryFileUnit,*, iostat=iostatus) &
               (SubreachBankStorageReleasedDuringRecovery(CurrentSubreach,CurrentRRF,CurrentNativeFlow), &
               CurrentNativeFlow=1,LengthOfBankStorageRecoveryPeriod+1)
            if (iostatus /= 0) then      !  Trap error on file read
                write (TextOutputFileUnit,1020) &
                'Bank storage recovery file ("both_recov.fil") incomplete in subroutine ReadBankStorageLossRecoveryTable.'
                InputErrorFlag = 1
                return
            end if  
     
     end do LoopThroughAndReadInputRRFReleases
     !Read channel loss and gain values for each subreach.
     CheckWhetherMonumentOrFountainCreekSubreach: if (CurrentSubreach <= NumberOfMonumentCreekSubreaches) then
          read (BankStorageRecoveryFileUnit,*, iostat=iostatus) &
               SubreachChannelStorageGainAlongMonumentCreek(CurrentSubreach), &
               SubreachChannelStorageLossAlongMonumentCreek(CurrentSubreach)
            if (iostatus /= 0) then      !  Trap error on file read
                write (TextOutputFileUnit,1020) &
                'Bank storage recovery file ("both_recov.fil") incomplete in subroutine ReadBankStorageLossRecoveryTable.'
                InputErrorFlag = 1
                return
            end if  
     
     else
          read (BankStorageRecoveryFileUnit,*, iostat=iostatus) &
               SubreachChannelStorageGainAlongFountainCreek(CurrentSubreach-NumberOfMonumentCreekSubreaches), &
               SubreachChannelStorageLossAlongFountainCreek(CurrentSubreach-NumberOfMonumentCreekSubreaches)
            if (iostatus /= 0) then      !  Trap error on file read
                write (TextOutputFileUnit,1020) &
                'Bank storage recovery file ("both_recov.fil") incomplete in subroutine ReadBankStorageLossRecoveryTable.'
                InputErrorFlag = 1
                return
            end if  
     
     end if CheckWhetherMonumentOrFountainCreekSubreach
end do LoopThroughAndReadStreamSubreachNumber

rewind (BankStorageRecoveryFileUnit)

1000 format (a80)
1010 format (a12)
1020 format (a100,/)

return
end

!*****************************************************************************
subroutine EstimateMonumentCreekRRFTransitLosses (NetRRFQInMonumentDischargedToFountain, &
     NetNativeQInMonumentDischargedToFountain,InputErrorFlag)
!*****************************************************************************
!Subroutine computes (estimates) transit-losses for reusable return flows along Monument Creek. Numerous other subroutines are called.

!Definition of local variables:
!-----------------------------------------------------------------------------
!LastSubreachInStreamSegment = the ending subreach within a stream segment (between gaging stations).
!NumberOfIterationsForConvergence = the number of iterations made for the stream segment and subreach loops before the convergence criterion is met.
!NetNativeQInMonumentDischargedToFountain = the net (total) amount of native flow at the end of Monument Creek reach carried forward to Fountain Creek reach.
!NetRRFQInMonumentDischargedToFountain = the net (total) amount of reusable return flow at the end of Monument Creek reach carried forward to Fountain Creek reach.
!BeginningSubreachInStreamSegment = the beginning subreach in a stream segment (between gaging stations).
!*****************************************************************************
use CommonVariables, &
     only : BankStorageRecoveryFileUnit,ConvergenceFlag,DownstreamReusableReturnFlowByRRFEntity, &
     FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow,FlagForScratchOutput,FlagForSubreachOrSegmentLevel, &
     FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions,GagingStationDischarge_Mainstem, &
     MinimumRRFInCFSForBankStorageToBeConsideredNotEqualTo0,MinimumRRFInCFSForUsingRegularBankStorageLookupTable, &
     MonumentCreekGagingStationNode,MonumentCreekSegmentLength,MonumentCreekSubreachLength,NameOfCreek, &
     NumberOfMonumentCreekSegments,NumberOfMonumentCreekSubreaches,&
     NumberOfReusableReturnFlowsEntered,NumberSubreachesNativeDiversionsGTNativeFlow,&
     PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest,ReleasedReusableReturnFlowName,&
     SCRATCH1FileUnit,SCRATCH2FileUnit,SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow,&
     SegmentDifferenceComputedDownstreamQCurrentAndLastIteration,SegmentDownstreamTotalGagedFlow, &
     SegmentDownstreamTotalNativeFlow,&
     SegmentGagedMainstemFlow,SegmentGagedTributaryFlow,SegmentGainOrLossInNativeFlowBetweenGagingStations, &
     SegmentNativeFlowGainOrLossPerMile,SegmentSumNativeFlowDiversionsAlongMonumentCreek,SegmentUpstreamTotalGagedFlow, &
     SegmentUpstreamTotalNativeFlow,SubreachBankStorageGainAlongMonumentCreek, &
     SubreachBankStorageLossAlongMounumentCreek, &
     SubreachBankStorageGainForEachRRFRelease,SubreachBankStorageGainMinusLoss, &
     SubreachBankStorageLossForEachRRFRelease,SubreachChannelStorageGain, &
     SubreachChannelStorageLoss, &
     SubreachDownstreamNativeFlowAlongMonumentCreek, &
     SubreachDownstreamTotalRRFAlongMonumentCreek,SubreachesNativeFlowLessThan0, &
     SubreachEvaporationLossMonumentCreek,SubreachGagedMainstemFlow,SubreachGagedTributaryFlow, &
     SubreachGainOrLossAllReusableFlowAlongMonumentCreek, &
     SubreachGainOrLossNativeFlowAlongMonumentCreek,SubreachGainOrLossNativeFlowAlongMonumentCreek_pct, &
     SubreachGainOrLossReusableFlowAlongMonumentCreek_pct,SubreachSumNativeFlowDiversionsAlongMonumentCreek, &
     SubreachSumReusableFlowConvertedToNativeFlow, &
     SubreachUpstreamNativeFlowAlongMonumentCreek, &
     SubreachUpstreamTotalRRFAlongMonumentCreek,TemporarySubreachDownstreamNativeFlow,TemporarySubreachDownstreamNativeFlow2, &
     TemporarySubreachDownstreamTotalRRF,TemporarySubreachDownstreamTotalRRF2,TemporarySubreachUpstreamNativeFlow, &
     TemporarySubreachUpstreamNativeFlow2,TemporarySubreachUpstreamTotalRRF,TemporarySubreachUpstreamTotalRRF2, &
     TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ,UpstreamReusableReturnFlowByRRFEntity, iostatus
     
implicit none

!#Create an interface so that you can pass an optional argument to subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach.
interface
     subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach(TotalRRFForCurrentSubreach,CurrentSubreach, &
         CurrentSegment,NumberOfIterationsForConvergence,PerformTransitLossSavingsComputation)
         integer :: CurrentSubreach,CurrentSegment,NumberOfIterationsForConvergence,CurrentRRFInSubreach, &
              CurrentRRFDiversion, CurrentRRFEntity,CurrentRRFAtNode,CurrentRRF
	      logical, intent(in), optional ::    PerformTransitLossSavingsComputation
	      real :: TotalRRFForCurrentSubreach
     end subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach
end interface

integer, intent(out) :: InputErrorFlag
integer :: BeginningSubreachInStreamSegment,LastSubreachInStreamSegment,NumberOfIterationsForConvergence, &
     CurrentSegmentNumber,CurrentSubreach,CurrentRRF,Loop21UntilConverge

real :: NetRRFQInMonumentDischargedToFountain,NetNativeQInMonumentDischargedToFountain,TemporaryRRF

!data SubreachBankStorageGainAlongMonumentCreek,SubreachBankStorageLossAlongMounumentCreek,SubreachDownstreamNativeFlowAlongMonumentCreek,SubreachDownstreamTotalRRFAlongMonumentCreek,SubreachEvaporationLossMonumentCreek,SubreachGainOrLossNativeFlowAlongMonumentCreek,SubreachGainOrLossAllReusableFlowAlongMonumentCreek,SubreachUpstreamNativeFlowAlongMonumentCreek,SubreachUpstreamTotalRRFAlongMonumentCreek,SubreachGainOrLossReusableFlowAlongMonumentCreek_pct,SubreachGainOrLossNativeFlowAlongMonumentCreek_pct /220*0.0/


allocate (character(3) :: SubreachesNativeFlowLessThan0(20))


InputErrorFlag = 0
NameOfCreek='MON'
FlagForScratchOutput=.TRUE.
NumberSubreachesNativeDiversionsGTNativeFlow=0

!Initialize /calc_var/ common block (selected variables) in "tmp_calcvar.cmn" include file.
FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=1.0
SegmentNativeFlowGainOrLossPerMile=0.0
PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest=0.0
SubreachBankStorageGainMinusLoss=0.0
SubreachGagedTributaryFlow=0.0
SubreachGagedMainstemFlow=0.0
SegmentDownstreamTotalNativeFlow=0.0
SegmentDownstreamTotalGagedFlow=0.0
SegmentGainOrLossInNativeFlowBetweenGagingStations=0.0
SegmentUpstreamTotalNativeFlow=0.0
SegmentUpstreamTotalGagedFlow=0.0
SegmentGagedTributaryFlow=0.0
SegmentGagedMainstemFlow=0.0
TemporarySubreachDownstreamNativeFlow=0.0
TemporarySubreachDownstreamNativeFlow2=0.0
TemporarySubreachDownstreamTotalRRF=0.0
TemporarySubreachDownstreamTotalRRF2=0.0
TemporarySubreachUpstreamNativeFlow=0.0
TemporarySubreachUpstreamNativeFlow2=0.0
TemporarySubreachUpstreamTotalRRF=0.0
TemporarySubreachUpstreamTotalRRF2=0.0

!Segment loop, from one gage downstream to next gage. repeat each segment loop (includes one or more subreaches) until assumed and computed
!downstream streamflows converge.
LoopThroughStreamSegments: do CurrentSegmentNumber=1,NumberOfMonumentCreekSegments
     !Reinitialize variables for the current segment.
     TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ=0.0
     SegmentDifferenceComputedDownstreamQCurrentAndLastIteration=0.0
     SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow=0.0
     NumberOfIterationsForConvergence=0
     ConvergenceFlag=.FALSE.
     !Set variables for starting and ending subreach numbers in current segment.
     CheckSegmentNumberAndAssignStartAndEndSubreachNumbers: if (CurrentSegmentNumber == 1) then
          BeginningSubreachInStreamSegment=1
          LastSubreachInStreamSegment=MonumentCreekGagingStationNode(CurrentSegmentNumber+1)-1
     else
          BeginningSubreachInStreamSegment=LastSubreachInStreamSegment+1
          LastSubreachInStreamSegment=MonumentCreekGagingStationNode(CurrentSegmentNumber+1)-1
     end if CheckSegmentNumberAndAssignStartAndEndSubreachNumbers
     !Set total discharge in Monument Creek at upstream and downstream gages of segment.
     SegmentUpstreamTotalGagedFlow=GagingStationDischarge_Mainstem(CurrentSegmentNumber)
     SegmentDownstreamTotalGagedFlow=GagingStationDischarge_Mainstem(CurrentSegmentNumber+1)
     !Set value of reusable return flow at the start of the current segment.
     !Set to zero for segment 1. For other segments, value is the ending value from previous segment.
     CheckForFirstSegmentAndAssignRRF: if (CurrentSegmentNumber == 1) then
          SubreachUpstreamTotalRRFAlongMonumentCreek(BeginningSubreachInStreamSegment)=0.0
          TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongMonumentCreek(BeginningSubreachInStreamSegment)
     else
          SubreachUpstreamTotalRRFAlongMonumentCreek(BeginningSubreachInStreamSegment)= &
               SubreachDownstreamTotalRRFAlongMonumentCreek(BeginningSubreachInStreamSegment-1)
          TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongMonumentCreek(BeginningSubreachInStreamSegment)
     end if CheckForFirstSegmentAndAssignRRF

     !Add any new reusable return flows in current segment and subtract any diversions 
     !(take-outs) of reusable return flows for exchange, off-stream
     !storage, or other forms of removal (subroutine 'ComputeAdjustedSegmentRRF'). 
     !Results in a modified value for "TemporarySubreachUpstreamTotalRRF."
     call ComputeAdjustedSegmentRRF (TemporarySubreachUpstreamTotalRRF,CurrentSegmentNumber,MonumentCreekGagingStationNode)

     !Determine if any there are any gaged tributary flows and any measured native return flows in the current segment.
     FlagForSubreachOrSegmentLevel=.FALSE.
     call ComputeTotalNativeRRFOrTributaryInflow (SegmentGagedTributaryFlow,SegmentGagedMainstemFlow,CurrentSegmentNumber, &
          MonumentCreekGagingStationNode)

     !Calculate US native flow on basis of reusable return flow, native return flow, and tributary flow. US native flow is now set and will not be
     !modified any further for subsequent subreach iterations within !current segment. Downstream native flow can be recalculated.
     SegmentUpstreamTotalNativeFlow=SegmentUpstreamTotalGagedFlow-TemporarySubreachUpstreamTotalRRF+ &
          (SegmentGagedTributaryFlow+SegmentGagedMainstemFlow)

     !Loop back to here if transit loss calculations have not converged.
     10 continue
     
      !Calculate conditional native flow downstream station. Assume Downstream reusable return flow is same as us value and add native diversions back in 
      !(gage data reflects the diversion). Also add in difference between assumed Downstream flows and calculated flows after each iteration 
      !(SegmentDifferenceComputedDownstreamQCurrentAndLastIteration, zero initially, is an adjustment to downstream native flow).
      SegmentDownstreamTotalNativeFlow=SegmentDownstreamTotalGagedFlow-TemporarySubreachUpstreamTotalRRF+ &
            SegmentDifferenceComputedDownstreamQCurrentAndLastIteration+ &
            SegmentSumNativeFlowDiversionsAlongMonumentCreek(CurrentSegmentNumber)

      !Calculate total gain/loss this segment.
      SegmentGainOrLossInNativeFlowBetweenGagingStations=SegmentDownstreamTotalNativeFlow-SegmentUpstreamTotalNativeFlow

      !Set segment channel length. Calculate native gain/loss per mile.
      SegmentNativeFlowGainOrLossPerMile=SegmentGainOrLossInNativeFlowBetweenGagingStations/ &
            MonumentCreekSegmentLength(CurrentSegmentNumber)

      !Inner loop. Loop through each of n subreaches within each segment.
      LoopThroughSubreachesInCurrentStreamSegment: &
      do CurrentSubreach=BeginningSubreachInStreamSegment,LastSubreachInStreamSegment

            !Write subreach number to scratch1.
            if (ConvergenceFlag) write (SCRATCH1FileUnit,1000) CurrentSubreach

            !Set value for reusable return flow at upstream node of current subreach. For subreach 1, value is from beginning of segment loop; otherwise,
            !from downstream node of previous subreach.
            CheckForFirstSubreachAndAssignUpstreamRRF:if (CurrentSubreach == 1) then
                  SubreachUpstreamTotalRRFAlongMonumentCreek(CurrentSubreach)= &
                         SubreachUpstreamTotalRRFAlongMonumentCreek(BeginningSubreachInStreamSegment)
                  TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongMonumentCreek(CurrentSubreach)
            else
                  SubreachUpstreamTotalRRFAlongMonumentCreek(CurrentSubreach)= &
                         SubreachDownstreamTotalRRFAlongMonumentCreek(CurrentSubreach-1)
                  TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongMonumentCreek(CurrentSubreach)
            end if CheckForFirstSubreachAndAssignUpstreamRRF

            !Determine if any new reusable return flows or diversions (take-outs) of reusable return flows for exchange, off-stream storage, or other forms
            !of removal are in current subreach (subroutine 'ComputePercentRRFForEachRRFEntityAtTopOfSubreach'). Results in an adjusted value of "TemporarySubreachUpstreamTotalRRF."
            
            call ComputePercentRRFForEachRRFEntityAtTopOfSubreach (TemporarySubreachUpstreamTotalRRF,CurrentSubreach, &
                  MonumentCreekGagingStationNode(CurrentSegmentNumber),NumberOfIterationsForConvergence)

            !Set additional variable to use in call statements (set to zero when flow is negative).
            TemporarySubreachUpstreamTotalRRF2=TemporarySubreachUpstreamTotalRRF
            if (TemporarySubreachUpstreamTotalRRF <= 0.0) TemporarySubreachUpstreamTotalRRF2=0.0

            !Determine if there are any gaged tributary flows and/or any measured native return flows in the current subreach.
            FlagForSubreachOrSegmentLevel=.TRUE.
            call ComputeTotalNativeRRFOrTributaryInflow &
                 (SubreachGagedTributaryFlow,SubreachGagedMainstemFlow,CurrentSubreach, MonumentCreekGagingStationNode)

            !Set native flow at upstream node.
            CheckForFirstSubreachAndAssignUpstreamNativeFlow: if (CurrentSubreach == 1) then
                  SubreachUpstreamNativeFlowAlongMonumentCreek(CurrentSubreach)=SegmentUpstreamTotalNativeFlow
                  TemporarySubreachUpstreamNativeFlow=SubreachUpstreamNativeFlowAlongMonumentCreek(CurrentSubreach)
            else
                  SubreachUpstreamNativeFlowAlongMonumentCreek(CurrentSubreach)= &
                         SubreachDownstreamNativeFlowAlongMonumentCreek(CurrentSubreach-1)
                  TemporarySubreachUpstreamNativeFlow=SubreachUpstreamNativeFlowAlongMonumentCreek(CurrentSubreach)
            end if CheckForFirstSubreachAndAssignUpstreamNativeFlow

            !Calculate adjusted native flow at upstream end of subreach on basis of tributary flow, native return flow, and native diversion. Also add in 
            !value of reusable return flow to be delivered in current subreach; these flows are converted to native flow. Set additional variable to use 
            !in call statements(set to zero when flow is negative).
            TemporarySubreachUpstreamNativeFlow=TemporarySubreachUpstreamNativeFlow+(SubreachGagedTributaryFlow + &
                  SubreachGagedMainstemFlow+SubreachSumReusableFlowConvertedToNativeFlow(CurrentSubreach))- &
                  SubreachSumNativeFlowDiversionsAlongMonumentCreek(CurrentSubreach)
            TemporarySubreachUpstreamNativeFlow2=TemporarySubreachUpstreamNativeFlow
            if (TemporarySubreachUpstreamNativeFlow <= 0.0) TemporarySubreachUpstreamNativeFlow2=0.0

            !Calculate native gain/loss this subreach and native flow at downstream node. Set additional variable to use in call statements (set to zero 
            !when flow is negative).
            SubreachGainOrLossNativeFlowAlongMonumentCreek(CurrentSubreach)=SegmentNativeFlowGainOrLossPerMile* &
                  MonumentCreekSubreachLength(CurrentSubreach)
            SubreachDownstreamNativeFlowAlongMonumentCreek(CurrentSubreach)=TemporarySubreachUpstreamNativeFlow+ &
                  SubreachGainOrLossNativeFlowAlongMonumentCreek(CurrentSubreach)
            TemporarySubreachDownstreamNativeFlow=SubreachDownstreamNativeFlowAlongMonumentCreek(CurrentSubreach)
            TemporarySubreachDownstreamNativeFlow2=TemporarySubreachDownstreamNativeFlow
            if (TemporarySubreachDownstreamNativeFlow <= 0.0) TemporarySubreachDownstreamNativeFlow2=0.0

            !Check if native streamflow less than zero and save for output report.
            CheckForNegativeDownstreamSubreachNativeFlow: &
            if (SubreachSumNativeFlowDiversionsAlongMonumentCreek(CurrentSubreach) > &
                  0.0 .and. TemporarySubreachDownstreamNativeFlow <= 0.0 .and. ConvergenceFlag) then
                  NumberSubreachesNativeDiversionsGTNativeFlow=NumberSubreachesNativeDiversionsGTNativeFlow+1
                  
                  CheckForMoreThan1SubreachWithNegativeDownstreamNativeFlow: &
                  if (NumberSubreachesNativeDiversionsGTNativeFlow > 1) then
                      write (SubreachesNativeFlowLessThan0(NumberSubreachesNativeDiversionsGTNativeFlow),1010) &
                      CurrentSubreach
                  else
                      write (SubreachesNativeFlowLessThan0(NumberSubreachesNativeDiversionsGTNativeFlow),1020) &
                      CurrentSubreach
                  end if CheckForMoreThan1SubreachWithNegativeDownstreamNativeFlow
                  
            end if CheckForNegativeDownstreamSubreachNativeFlow

            !Look up bank storage loss (SubreachBankStorageLossAlongMounumentCreek) for adjusted upstream reusable and native flows. If TemporarySubreachUpstreamTotalRRF2 < 0.5 skip subroutine.
            !Program modified 03/2012 to add second lookup table for Monument Creek to provide more resolution for BS loss for very small 
            !reusable return flows.
            !if (TemporarySubreachUpstreamTotalRRF2 .gt. 0.5) then
            !     call InterpolateBankStorageLoss (SubreachBankStorageLossAlongMounumentCreek(CurrentSubreach),CurrentSubreach)
            !else
            !     SubreachBankStorageLossAlongMounumentCreek(CurrentSubreach)=0.0
            !end if

            !Look up bank storage loss (SubreachBankStorageLossAlongMounumentCreek) for adjusted upstream reusable and native flows. If TemporarySubreachUpstreamTotalRRF2 < 0.05 skip subroutine.
            CheckIfHighResolutionLookupTableShouldBeUsed: if (TemporarySubreachUpstreamTotalRRF2 > &
                  MinimumRRFInCFSForUsingRegularBankStorageLookupTable) then
                  call InterpolateBankStorageLoss &
                    (SubreachBankStorageLossAlongMounumentCreek(CurrentSubreach),CurrentSubreach)
            else if (TemporarySubreachUpstreamTotalRRF2 <= MinimumRRFInCFSForUsingRegularBankStorageLookupTable) then
                  CheckForUpstreamRRFSoSmallThatBSLossIs0: if (TemporarySubreachUpstreamTotalRRF2 >=  &
                         MinimumRRFInCFSForBankStorageToBeConsideredNotEqualTo0) then
                         call InterpolateBankStorageLossForLowMonumentRRF &
                             (SubreachBankStorageLossAlongMounumentCreek(CurrentSubreach), CurrentSubreach)
                  else if (TemporarySubreachUpstreamTotalRRF2 < MinimumRRFInCFSForBankStorageToBeConsideredNotEqualTo0) then
                         SubreachBankStorageLossAlongMounumentCreek(CurrentSubreach)=0.0
                  end if CheckForUpstreamRRFSoSmallThatBSLossIs0
            end if CheckIfHighResolutionLookupTableShouldBeUsed

            !Calculate adjustment factor for bank storage loss (SubreachBankStorageLossAlongMounumentCreek).
            CheckForNonZeroNativeFlowAndBSLossAdjustmentFactorNE1: if (TemporarySubreachUpstreamNativeFlow2 > 0.0 .and. &
                  TemporarySubreachDownstreamNativeFlow2 > 0.0) then
                  call AdjustBankStorageLossForNonuniformNativeFlow (CurrentSubreach)
            else
                  FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=1.0
            end if CheckForNonZeroNativeFlowAndBSLossAdjustmentFactorNE1

            !Calculate adjusted bank storage loss.
            SubreachBankStorageLossAlongMounumentCreek(CurrentSubreach) = &
                 SubreachBankStorageLossAlongMounumentCreek(CurrentSubreach)* &
                  FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow

            !Calculate total gains from bank storage. Set recovery period in "call." 
            !add losses and gains to determine net bank storage loss/gain (SubreachBankStorageGainMinusLoss).

            !Calculate bank storage loss and gains for individual reusable return flows. 
            ! Method retains use of subreach values for lump sum of return 
            ! flows while calculating individual values. 
            ! L. Miller 9-02-2009

            !Set bank storage gain for subreach to zero. Then calculate the bank storage 
            !loss for each individual return flow by applying appropriate percentage value
            !to the total subreach value for lump sum of all return flows.

            SubreachBankStorageGainAlongMonumentCreek(CurrentSubreach)=0.0
            LoopThroughRRFsToEstimateNetBankStorageGainInSubreach: do CurrentRRF=1,NumberOfReusableReturnFlowsEntered
                  SubreachBankStorageLossForEachRRFRelease(CurrentSubreach,CurrentRRF)= &
                         SubreachBankStorageLossAlongMounumentCreek(CurrentSubreach)* &
                         (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)
                  if (ConvergenceFlag) write (SCRATCH1FileUnit,1030) CurrentRRF,ReleasedReusableReturnFlowName(CurrentRRF)
                  !Now call subroutine to compute cumulative gains from bank storage for each individual return flow for the appropriate number of days 
                  !in recovery period.
                  call ComputeBankStorageRecovery (SubreachBankStorageLossForEachRRFRelease(CurrentSubreach,CurrentRRF), &
                         SubreachBankStorageGainForEachRRFRelease(CurrentSubreach,CurrentRRF),CurrentSubreach,CurrentRRF)
                  !Lastly, sum individual bank storage gains to get total value for all return flows in the subreach.
                  SubreachBankStorageGainAlongMonumentCreek(CurrentSubreach)= &
                         SubreachBankStorageGainAlongMonumentCreek(CurrentSubreach)+ &
                         SubreachBankStorageGainForEachRRFRelease(CurrentSubreach,CurrentRRF)
            end do LoopThroughRRFsToEstimateNetBankStorageGainInSubreach

            SubreachBankStorageGainMinusLoss=SubreachBankStorageGainAlongMonumentCreek(CurrentSubreach)- &
                  SubreachBankStorageLossAlongMounumentCreek(CurrentSubreach)

            !Calculate first temporary Downstream reusable return flow--used to estimate channel storage loss.

            TemporarySubreachDownstreamTotalRRF=TemporarySubreachUpstreamTotalRRF+ &
                  SubreachBankStorageGainMinusLoss

            !Set additional variable to use in call statements (set to zero when flow is negative).

            TemporarySubreachDownstreamTotalRRF2=TemporarySubreachDownstreamTotalRRF
            if (TemporarySubreachDownstreamTotalRRF <= 0.0) TemporarySubreachDownstreamTotalRRF2=0.0

            !Calculate channel storage loss and gain.
            SubreachChannelStorageLoss=0.0
            SubreachChannelStorageGain=0.0
            call ComputeSubreachChannelStorageLoss (CurrentSubreach)

            !Calculate second temporary Downstream reusable return flow using channel storage loss/gain.
            TemporarySubreachDownstreamTotalRRF=TemporarySubreachDownstreamTotalRRF+SubreachChannelStorageGain- &
                  SubreachChannelStorageLoss
            TemporarySubreachDownstreamTotalRRF2=TemporarySubreachDownstreamTotalRRF
            if (TemporarySubreachDownstreamTotalRRF <= 0.0) TemporarySubreachDownstreamTotalRRF2=0.0

            !Calculate evaporative loss. if reusable flow zero or less, no evap. loss.
            call CalculateRRFEvaporativeLoss (SubreachEvaporationLossMonumentCreek(CurrentSubreach), &
                  MonumentCreekSubreachLength(CurrentSubreach))

            !Calculate total reusable gain/loss this subreach and reusable return flow at downstream node. 
            !Set value of downstream native flow.
            CheckForConvergenceAndCalculateDownstreamRRFForSubreach: if (.not. ConvergenceFlag) then
                  SubreachGainOrLossAllReusableFlowAlongMonumentCreek(CurrentSubreach)= &
                         (SubreachBankStorageGainMinusLoss+SubreachChannelStorageGain)- &
                         (SubreachChannelStorageLoss+SubreachEvaporationLossMonumentCreek(CurrentSubreach))
                  SubreachDownstreamTotalRRFAlongMonumentCreek(CurrentSubreach)=TemporarySubreachUpstreamTotalRRF+ &
                         SubreachGainOrLossAllReusableFlowAlongMonumentCreek(CurrentSubreach)
                  SubreachDownstreamNativeFlowAlongMonumentCreek(CurrentSubreach)=TemporarySubreachDownstreamNativeFlow

                  !Calculate downstream flow quantities for each individual reusable return-flow entity 
                  !for current subreach on basis of percentages
                  !computed at beginning of subreach. Only when converged.

            !Calculate channel storage loss/gain and evaporative loss for each individual return 
            !flow using appropriate percentage value calculated 
            !earlier. Sum all transit loss/gain values for each individual return flow to get 
            !total transit loss/gain values for current subreach.
            else if (ConvergenceFlag) then
                  SubreachGainOrLossAllReusableFlowAlongMonumentCreek(CurrentSubreach)=0.0
                  
                  LoopThroughAllRRFsToCalculateTotalTransitLossForCurrentSubreach: &
                  do CurrentRRF=1,NumberOfReusableReturnFlowsEntered
                         TemporaryRRF=SubreachBankStorageGainForEachRRFRelease(CurrentSubreach,CurrentRRF) - &
                              SubreachBankStorageLossForEachRRFRelease(CurrentSubreach,CurrentRRF) + &
                              SubreachChannelStorageGain * &
                              (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.) - &
                              SubreachChannelStorageLoss * &
                              (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.) &
                              + SubreachEvaporationLossMonumentCreek(CurrentSubreach) * &
                              (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)
                         !Calculate downstream values for individual return flow entities and 
                         !subreach value of total transit loss/gain.
                         DownstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRF)= &
                              UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRF)+TemporaryRRF
                         UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach+1,CurrentRRF)= &
                              DownstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRF)
                         SubreachGainOrLossAllReusableFlowAlongMonumentCreek(CurrentSubreach)= &
                              SubreachGainOrLossAllReusableFlowAlongMonumentCreek(CurrentSubreach)+TemporaryRRF
                  end do LoopThroughAllRRFsToCalculateTotalTransitLossForCurrentSubreach
                  
                  !Calculate net values at downstream node of current subreach.
                  SubreachDownstreamTotalRRFAlongMonumentCreek(CurrentSubreach)=TemporarySubreachUpstreamTotalRRF+ &
                         SubreachGainOrLossAllReusableFlowAlongMonumentCreek(CurrentSubreach)
                  SubreachDownstreamNativeFlowAlongMonumentCreek(CurrentSubreach)=TemporarySubreachDownstreamNativeFlow
            end if CheckForConvergenceAndCalculateDownstreamRRFForSubreach

            !When converged, calculate percentage reusable return flow loss/gain and 
            !percentage native loss/gain for each subreach (display on output).
            CheckForConvergenceAndCalculatePercentNetRRFGainForSubreach: if (ConvergenceFlag) then
            
                  CheckForNonZeroNetRRFGain: if (abs(TemporarySubreachUpstreamTotalRRF) > 0.0) then
                         SubreachGainOrLossReusableFlowAlongMonumentCreek_pct(CurrentSubreach)= &
                               (SubreachGainOrLossAllReusableFlowAlongMonumentCreek(CurrentSubreach)/ &
                               abs(TemporarySubreachUpstreamTotalRRF))*100.
                  else
                         SubreachGainOrLossReusableFlowAlongMonumentCreek_pct(CurrentSubreach)=0.0
                  end if CheckForNonZeroNetRRFGain
                  
                  CheckForNonZeroNetNativeFlowGain: if (abs(TemporarySubreachUpstreamNativeFlow) > 0.0) then
                         SubreachGainOrLossNativeFlowAlongMonumentCreek_pct(CurrentSubreach)= &
                               (SubreachGainOrLossNativeFlowAlongMonumentCreek(CurrentSubreach)/ &
                               abs(TemporarySubreachUpstreamNativeFlow))*100.
                  else
                         SubreachGainOrLossNativeFlowAlongMonumentCreek_pct(CurrentSubreach)=0.0
                  end if CheckForNonZeroNetNativeFlowGain
            end if CheckForConvergenceAndCalculatePercentNetRRFGainForSubreach

      !End of subreach loop.
      end do LoopThroughSubreachesInCurrentStreamSegment

      !Calculate difference between assumed Downstream flow and calculated Downstream flow after subreach computations (segment loop).
      TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ = &
            SubreachDownstreamTotalRRFAlongMonumentCreek(CurrentSubreach-1) + &
            SubreachDownstreamNativeFlowAlongMonumentCreek(CurrentSubreach-1)
      SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow=SegmentDownstreamTotalGagedFlow - &
            TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ
      CheckForNonZeroFlowAtDownstreamEndOfSegment: if (SegmentDownstreamTotalGagedFlow > 0.0) then
            PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest= &
                  SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow/SegmentDownstreamTotalGagedFlow*100.
      else
            PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest=0.0
      end if CheckForNonZeroFlowAtDownstreamEndOfSegment

      !See if difference is within 0.5% criterium. If not converged, loop back into 
      !segment computation without resetting/incrementing upstream variables.
      !Note that a final set of subreach iterations is made after segment iterations converge. 
      !Last iteration is needed so that bank-storage and channel 
      !storage values can be written to appropriate files and that downstream/upstream 
      !percentages of each reusable return-flow entity can be calculated.
      
      CheckForFailureToConverge: &
      if  (.not. ConvergenceFlag) then
            CheckForMoreThan20Iterations: if (NumberOfIterationsForConvergence >= 20) then
                  ConvergenceFlag=.TRUE.
                  write (SCRATCH2FileUnit,1040) CurrentSegmentNumber
             else if (abs(PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest) < 0.1) then
                  ConvergenceFlag=.TRUE.
                  NumberOfIterationsForConvergence=NumberOfIterationsForConvergence+1
             else if (abs(PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest) >= 0.1) then
                  SegmentDifferenceComputedDownstreamQCurrentAndLastIteration= &
                         SegmentDifferenceComputedDownstreamQCurrentAndLastIteration+ &
                         SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow
                  NumberOfIterationsForConvergence=NumberOfIterationsForConvergence+1
            end if CheckForMoreThan20Iterations
            
            goto 10   
      end if CheckForFailureToConverge

!End of segment loop.
end do LoopThroughStreamSegments

!Set variables for the amount of reusable return and native flow at station 07105500 
!(at Nevada Street gage) from Monument Creek reach to be carried into calculations for Fountain Creek reach.
NetRRFQInMonumentDischargedToFountain=SubreachDownstreamTotalRRFAlongMonumentCreek(CurrentSubreach-1)
NetNativeQInMonumentDischargedToFountain=SubreachDownstreamNativeFlowAlongMonumentCreek(CurrentSubreach-1)

return

1000 format ('SUBREACH ',i2.2)
1010 format (',',i2)
1020 format (' ',i2)
1030 format ('RU return flow ',i3.3,': ', a56)
1040 format ('ICCHK convergence criteria not met after 20 iterations in Monument Creek segment',i2,'.')

end

!*****************************************************************************
subroutine ComputeAdjustedSegmentRRF (TotalRRFInSegment,CurrentSegment,GagingStationNode)
!*****************************************************************************
!Subroutine computes an adjusted reusable return flow to begin a new stream segment.

!Definition of local variables:
!-----------------------------------------------------------------------------
!CurrentSegment = value for current segment, carried from calling subroutine.
!GagingStationNode = local array for the MonumentCreekGagingStationNode or FountainCreekGagingStationNode arrays from the calling subroutines.
!TotalRRFInSegment = value for the total reusable return flow, from calling subroutine.
!          adjusted locally, and carried back to calling subroutine.
!*****************************************************************************
use CommonVariables, &
     only : NumberOfReusableReturnFlowDiversions,NumberOfReusableReturnFlows,ReusableReturnFlowReleaseNode, &
     ReusableReturnFlowDischarge,ReusableReturnFlowDiversionNode,ReusableDiversionDischarge
implicit none

integer :: CurrentSegment,GagingStationNode(10),CurrentRRF,CurrentRRFDiversion
      
real :: TotalRRFInSegment



!Add in any reusable return flows (ReusableReturnFlowDischarge's) to value of total reusable return flow, TotalRRFInSegment.
LoopThroughAllUsedRRFs: do CurrentRRF=1,NumberOfReusableReturnFlows
     CheckForSegmentNumberGreaterThan1: if (ReusableReturnFlowReleaseNode(CurrentRRF) >= 1) then
          if (ReusableReturnFlowReleaseNode(CurrentRRF) >= GagingStationNode(CurrentSegment) .and. &
               ReusableReturnFlowReleaseNode(CurrentRRF) < GagingStationNode(CurrentSegment+1)) TotalRRFInSegment= &
                    TotalRRFInSegment+ReusableReturnFlowDischarge(CurrentRRF)
     end if CheckForSegmentNumberGreaterThan1
end do LoopThroughAllUsedRRFs

!Subtract any diversion (take-outs) of reusable return flows in the current segment.
LoopThroughAllDischargedRRFs: do CurrentRRFDiversion=1,NumberOfReusableReturnFlowDiversions
     if (ReusableReturnFlowDiversionNode(CurrentRRFDiversion) >= GagingStationNode(CurrentSegment) .and. &
          ReusableReturnFlowDiversionNode(CurrentRRFDiversion) < GagingStationNode(CurrentSegment+1)) TotalRRFInSegment= &
               TotalRRFInSegment-ReusableDiversionDischarge(CurrentRRFDiversion)
end do LoopThroughAllDischargedRRFs

return

end
!*****************************************************************************
subroutine ComputeTotalNativeRRFOrTributaryInflow (TributaryInflowForCurrentSegmentOrSubreach, &
     NativeReturnFlowForCurrentSegmentOrSubreach,CurrentSegmentOrSubreachNumber,GagingStationNode)
!*****************************************************************************
!Subroutine computes the total native return flow or tributary inflow in the current stream segment or subreach.

!Definition of local variables:
!-----------------------------------------------------------------------------
!CurrentSegmentOrSubreachNumber = local variable for the number of the current stream segment or subreach, from calling subroutine.
!GagingStationNode = local array for the MonumentCreekGagingStationNode or FountainCreekGagingStationNode arrays from the calling subroutines.
!TributaryInflowForCurrentSegmentOrSubreach = local name for total value of tributary inflow in current stream segment or subreach.
!NativeReturnFlowForCurrentSegmentOrSubreach = local name for total value of native return flow in current stream segment or subreach.
!*****************************************************************************
use CommonVariables, &
     only : GagingStationDischarge_Tributary,NativeReturnFlowDischarge,FlagForSubreachOrSegmentLevel, &
     NumberOfNativeReturnFlows,NumberOfTributaryGagingStations,TributaryGagingStationNode, &
     SubreachSumOfGagingStationDischarge_Tributary,GagingStationDischarge_Tributary,SubreachSumOfMeasuredNativeReturnFlows, &
     FlagForSubreachOrSegmentLevel,NumberOfNativeReturnFlows,NumberOfTributaryGagingStations,TributaryGagingStationNode, &
     NativeReturnFlowReleaseNode
implicit none

integer :: CurrentSegmentOrSubreachNumber,GagingStationNode(10),CurrentTributaryGagingStation,CurrentNativeReturnFlow
      
real :: TributaryInflowForCurrentSegmentOrSubreach,NativeReturnFlowForCurrentSegmentOrSubreach


!Sum any tributary flows that are in current segment or subreach. Quantity is used to calculate conditional native streamflow for
!downstream node of segment or subreach. Also sum tributary flow by subreach (SubreachSumOfGagingStationDischarge_Tributary(CurrentSegmentOrSubreachNumber)) for transit loss output report.

TributaryInflowForCurrentSegmentOrSubreach=0.0
SubreachSumOfGagingStationDischarge_Tributary(CurrentSegmentOrSubreachNumber)=0.0

LoopThroughAllTributaryFlows: do CurrentTributaryGagingStation=1,NumberOfTributaryGagingStations
    
     CheckWhetherSegmentOrSubreachTribFlow: if (.not. FlagForSubreachOrSegmentLevel) then
          
          if (TributaryGagingStationNode(CurrentTributaryGagingStation) >= GagingStationNode(CurrentSegmentOrSubreachNumber) &
               .and. &
               TributaryGagingStationNode(CurrentTributaryGagingStation) < GagingStationNode(CurrentSegmentOrSubreachNumber+1)) &
                    TributaryInflowForCurrentSegmentOrSubreach = TributaryInflowForCurrentSegmentOrSubreach + &
                    GagingStationDischarge_Tributary(CurrentTributaryGagingStation)
     else if (FlagForSubreachOrSegmentLevel) then
         
          if (TributaryGagingStationNode(CurrentTributaryGagingStation) == CurrentSegmentOrSubreachNumber) then
          
               TributaryInflowForCurrentSegmentOrSubreach=TributaryInflowForCurrentSegmentOrSubreach+ &
                    GagingStationDischarge_Tributary(CurrentTributaryGagingStation)
               SubreachSumOfGagingStationDischarge_Tributary(CurrentSegmentOrSubreachNumber)= &
                    SubreachSumOfGagingStationDischarge_Tributary(CurrentSegmentOrSubreachNumber)+ &
                    GagingStationDischarge_Tributary(CurrentTributaryGagingStation)
          end if
     end if CheckWhetherSegmentOrSubreachTribFlow
end do LoopThroughAllTributaryFlows

!Sum any measured native return flows that are in current segment or subreach. 
!Quantity is used to calculate conditional native streamflow 
!for downstream node of segment or subreach. Also sum measured native return flows by subreach (SubreachSumOfMeasuredNativeReturnFlows(CurrentSegmentOrSubreachNumber)) for transit loss output report.
NativeReturnFlowForCurrentSegmentOrSubreach=0.0
SubreachSumOfMeasuredNativeReturnFlows(CurrentSegmentOrSubreachNumber)=0.0
LoopThroughAllNativeReturnFlows: do CurrentNativeReturnFlow=1,NumberOfNativeReturnFlows
     CheckWhetherSegmentOrSubreachNativeFlow: if (.not. FlagForSubreachOrSegmentLevel) then
          if (NativeReturnFlowReleaseNode(CurrentNativeReturnFlow) >= GagingStationNode(CurrentSegmentOrSubreachNumber) &
               .and. &
               NativeReturnFlowReleaseNode(CurrentNativeReturnFlow) < GagingStationNode(CurrentSegmentOrSubreachNumber+1)) &
                    NativeReturnFlowForCurrentSegmentOrSubreach = NativeReturnFlowForCurrentSegmentOrSubreach + &
                    NativeReturnFlowDischarge(CurrentNativeReturnFlow)
     else if (FlagForSubreachOrSegmentLevel) then
          if (NativeReturnFlowReleaseNode(CurrentNativeReturnFlow) == CurrentSegmentOrSubreachNumber) then
               NativeReturnFlowForCurrentSegmentOrSubreach = NativeReturnFlowForCurrentSegmentOrSubreach + &
                    NativeReturnFlowDischarge(CurrentNativeReturnFlow)
               SubreachSumOfMeasuredNativeReturnFlows(CurrentSegmentOrSubreachNumber) = &
                    SubreachSumOfMeasuredNativeReturnFlows(CurrentSegmentOrSubreachNumber) + &
                    NativeReturnFlowDischarge(CurrentNativeReturnFlow)
          end if
     end if CheckWhetherSegmentOrSubreachNativeFlow
end do LoopThroughAllNativeReturnFlows

return

end

!**************************************************************************************
subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach &
     (TotalRRFForCurrentSubreach,CurrentSubreach,CurrentSegment, &
     NumberOfIterationsForConvergence,PerformTransitLossSavingsComputation)
     
!**************************************************************************************
!Subroutine calculates the percentage of total reusable return flow for each reusable return flow entity at the beginning of each subreach.
!Adjustments are made to flow values for new inflows and diversions.

!Definition of local variables:
!-----------------------------------------------------------------------------
!CurrentSubreach = local variable for the number of the current subreach, from calling subroutine.
!TotalRRFForCurrentSubreach = local variable for the total reusable return flow, carried from calling subroutine, modified here, and then carried back.
!*****************************************************************************
use CommonVariables, &
     only : SubreachSumOfRRFReleases,SubreachSumOfRRFDiversions,SubreachSumReusableFlowConvertedToNativeFlow, &
     UpstreamReusableReturnFlowByRRFEntity,ReusableReturnFlowDischarge,TemporaryReusableDiversionDischarge, &
     ReusableDiversionDischarge,ReusableReturnFlowDischarge,RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions, &
     ConvergenceFlag,NumberOfMonumentCreekSubreaches,NumberOfReusableReturnFlowDiversions,NumberOfReusableReturnFlows, &
     SurrogateCSFryArkWWTFReturnFlowRelease,SurrogateCSReleaseFromUpstreamReservoir,SurrogateFortCarsonReturnFlowRelease, &
     SurrogateFutureCSWWTFReturnFlowRelease,SurrogateNWRFFryArkReleaseReachingWWTF,SurrogateNWRFFutureReleaseReachingWWTF, &
     SurrogateNWRFReturnFlowReleaseReachingWWTF,SurrogateWWTFReturnFlowRelease,DownstreamReusableReturnFlowByRRFEntity, &
     ReusableReturnFlowReleaseNode,ReusableReturnFlowDiversionNode,ReusableReturnFlowEntity,ReusableReturnFlowDeliveryNode, &
     FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions
implicit none

!#Create an interface so that you can pass an optional argument to subroutine CheckIfCSDiversionsLessThanRRF.
interface
     subroutine CheckIfCSDiversionsLessThanRRF (TotalRRFInSubreach,FountainCreekSubreach,PerformTransitLossSavingsComputation)
          real :: TotalRRFInSubreach,AddTLSDiversion
          integer :: FountainCreekSubreach,FountainCreekSubreachNumber
          logical, intent(in), optional :: PerformTransitLossSavingsComputation
     end subroutine CheckIfCSDiversionsLessThanRRF     
end interface

integer :: CurrentSubreach,CurrentSegment,NumberOfIterationsForConvergence,CurrentRRFInSubreach,CurrentRRFDiversion, &
     CurrentRRFEntity,CurrentRRFAtNode,CurrentRRF

!  PerformTransitLossSavingsComputation is set to true for calculating transitloss savings
logical, intent(in), optional:: PerformTransitLossSavingsComputation
      
real :: TotalRRFForCurrentSubreach

!At begin of second Fountain Creek subreach (at Las Vegas St. WWTF), set variable and value for Colo. Springs reusable return flows routed
!downstream from northern reclamation facility.
CheckForFountainCreekSubreach: if (CurrentSubreach == NumberOfMonumentCreekSubreaches+2) then
     SurrogateNWRFReturnFlowReleaseReachingWWTF=DownstreamReusableReturnFlowByRRFEntity(CurrentSubreach-1,1)
     SurrogateNWRFFryArkReleaseReachingWWTF=DownstreamReusableReturnFlowByRRFEntity(CurrentSubreach-1,2)
     SurrogateNWRFFutureReleaseReachingWWTF=DownstreamReusableReturnFlowByRRFEntity(CurrentSubreach-1,3)
end if CheckForFountainCreekSubreach
       
! Initialize.
! Not used when computing  TL savings.
CheckForSubroutineFlag: if (.not. present(PerformTransitLossSavingsComputation)) then
     SubreachSumOfRRFReleases(CurrentSubreach)=0.0
     SubreachSumOfRRFDiversions(CurrentSubreach)=0.0
end if CheckForSubroutineFlag
SubreachSumReusableFlowConvertedToNativeFlow(CurrentSubreach)=0.0

!For each subreach, add any additional reusable return flows in current subreach to value of total reusable return flow, TotalRRFForCurrentSubreach.
LoopThroughAllUsedRRFsInSubreachAndSum: do CurrentRRFInSubreach=1,NumberOfReusableReturnFlows
     CheckForUsedRRFLocatedOnCurrentSubreach: &
          if (ReusableReturnFlowReleaseNode(CurrentRRFInSubreach) == CurrentSubreach) then
          !Set subreach upstream value for each additional individual return flow.     
          UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFInSubreach) = &
               ReusableReturnFlowDischarge(CurrentRRFInSubreach)
          !Add in any new return flows in current subreach.          
          TotalRRFForCurrentSubreach = TotalRRFForCurrentSubreach + &
               UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFInSubreach) 
          !Sum reusable return flows by subreach for transit loss output report.
          ! Not used when computing TL savings.
          if (.not. present(PerformTransitLossSavingsComputation)) &
          SubreachSumOfRRFReleases(CurrentSubreach) = SubreachSumOfRRFReleases(CurrentSubreach) + &
               ReusableReturnFlowDischarge(CurrentRRFInSubreach)  
          
          !Make all necessary adjustments to Colo. Springs return flows, including adding of flows from northern recl. facility to flows
          !at Las Vegas St. WWTF.
          
          !Reminder for variables and values for Colo. Springs reusable return flows
          !at Las Vegas St. WWTF. that were set when return flows were input.
          !x      SurrogateCSReleaseFromUpstreamReservoir=ReusableReturnFlowDischarge(4)   SurrogateWWTFReturnFlowRelease=ReusableReturnFlowDischarge(5)
          !x      SurrogateCSFryArkWWTFReturnFlowRelease=ReusableReturnFlowDischarge(6)   SurrogateFutureCSWWTFReturnFlowRelease=ReusableReturnFlowDischarge(7)
          !x      SurrogateFortCarsonReturnFlowRelease=ReusableReturnFlowDischarge(8)

          !Sum Colo. Springs transmountain return flow and suppl. release at Las
          !Vegas St. WWTF and add transmountin from Monument Creek. Set array value for suppl. release to zero. variable "ReusableReturnFlowDischarge(4)" = "SurrogateCSReleaseFromUpstreamReservoCurrentRRFInSubreach."
          !Note that future return flow added to transmountain, but actual water type not know as of 03/28/2007.

          if (CurrentSubreach == NumberOfMonumentCreekSubreaches+2 .and. CurrentRRFInSubreach == 5) &
               UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,5)= &
                    SurrogateWWTFReturnFlowRelease + &
                    SurrogateCSReleaseFromUpstreamReservoir + &
                    SurrogateFutureCSWWTFReturnFlowRelease+ &
                    SurrogateNWRFReturnFlowReleaseReachingWWTF

          !Sum Colo. Springs Fry-Ark return flow at Las Vegas St. WWTF and FRY-ARK flow from Northern Recl. facility.
          if (CurrentSubreach == NumberOfMonumentCreekSubreaches+2 .and. CurrentRRFInSubreach == 6) &
               UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,6) = SurrogateCSFryArkWWTFReturnFlowRelease + &
                    SurrogateNWRFFryArkReleaseReachingWWTF

          !Add Ft. Carson return flow to Colorado Springs transmountain. Set array value for Ft. Carson return to zero. 
          !Variable "ReusableReturnFlowDischarge(8)" = "SurrogateFortCarsonReturnFlowRelease."
          if (CurrentSubreach == NumberOfMonumentCreekSubreaches+6 .and. CurrentRRFInSubreach == 8) &
               UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,5)= &
                    DownstreamReusableReturnFlowByRRFEntity(CurrentSubreach-1,5)+SurrogateFortCarsonReturnFlowRelease

    end if CheckForUsedRRFLocatedOnCurrentSubreach
end do LoopThroughAllUsedRRFsInSubreachAndSum

!Check if there are any reusable diversions in current subreach and subtract diversion (take-outs) from needed values.
LoopThroughAllRRFDiversions: do CurrentRRFDiversion=1,NumberOfReusableReturnFlowDiversions
     CheckForRRFDiversionsLocatedOnCurrentSubreach: &
          if (ReusableReturnFlowDiversionNode(CurrentRRFDiversion) == CurrentSubreach) then
          
          !Set diversion return-flow source.
          CurrentRRFEntity=ReusableReturnFlowEntity(CurrentRRFDiversion)
                    
       !If at beginning of stream segment loop after first iteration, any diversion subtracted during the previous iteration needs to be
       !added back in to the entities reusable flow before a new subtraction. This prevents the flow for the entity from going to zero 
       !in error. 
          !Not sure why this is happening, but this fix seems to work.
          if (NumberOfIterationsForConvergence >= 1 .and. CurrentSegment == CurrentSubreach) &
               UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity) = &
               UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity) + &
               TemporaryReusableDiversionDischarge(CurrentRRFDiversion)
          
          !Reset temporary diversion amount to input diversion amount.
          TemporaryReusableDiversionDischarge(CurrentRRFDiversion)=ReusableDiversionDischarge(CurrentRRFDiversion)

          !First check if reusable flow for entity at current subreach is sufficient for specified diversion. If not, adjust diversion to available reuse flow.
          CheckForSufficientFlowForRRFDiversion: if (UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity) <= &
               TemporaryReusableDiversionDischarge(CurrentRRFDiversion)) then
                    TemporaryReusableDiversionDischarge(CurrentRRFDiversion) = &
                         UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity)
                    UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity)= &
                         UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity) - &
                         TemporaryReusableDiversionDischarge(CurrentRRFDiversion)
          !If reusable flow sufficient, just subtract diversion from return flow.
          else if (UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity) > &
               TemporaryReusableDiversionDischarge(CurrentRRFDiversion)) then
                    UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity)= &
                         UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity) - &
                         TemporaryReusableDiversionDischarge(CurrentRRFDiversion)
          end if CheckForSufficientFlowForRRFDiversion

          !To prevent negative values from showing up.
          if (UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity) <= 0.0) & 
               UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFEntity)=0.0

          !Subtract diversions (take-outs) from total subreach value of reusable flow.
          TotalRRFForCurrentSubreach=TotalRRFForCurrentSubreach-TemporaryReusableDiversionDischarge(CurrentRRFDiversion)

          !Sum diversions by subreach for writing to transit loss output report. 
          ! Not used when computing TL savings.
          if (.not. present(PerformTransitLossSavingsComputation)) SubreachSumOfRRFDiversions(CurrentSubreach)= &
               SubreachSumOfRRFDiversions(CurrentSubreach)+TemporaryReusableDiversionDischarge(CurrentRRFDiversion)
          
     end if CheckForRRFDiversionsLocatedOnCurrentSubreach
end do LoopThroughAllRRFDiversions

!Determine reusable delivery flow disposition based on delivery nodes that are less than the last downstream node at mouth of Fountain Creek.
LoopThroughAllRRFsInSubreachAndEstimateDeliveryFlow: do CurrentRRFAtNode=1,NumberOfReusableReturnFlows
     CheckForRRFDeliveryNodeLocatedOnCurrentSubreach: &
     if (ReusableReturnFlowDeliveryNode(CurrentRRFAtNode) == CurrentSubreach) then
          
          !If delivery node is same as input node, set delivery flow equal to input flow. Value is assumed to be converted to native flow.
          CheckForDelveryNodeSameAsInputNode: if (ReusableReturnFlowDeliveryNode(CurrentRRFAtNode) == &
               ReusableReturnFlowReleaseNode(CurrentRRFAtNode)) then
               
               RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRFAtNode)=ReusableReturnFlowDischarge(CurrentRRFAtNode)
               
               !Subtract delivery discharge from subreach beginning reusable flow.
               if (CurrentRRFAtNode >= 9) TotalRRFForCurrentSubreach=TotalRRFForCurrentSubreach- &
                    RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRFAtNode)
               
               !Set value of reusable flow converted to native flow.
               if (CurrentRRFAtNode >= 9) SubreachSumReusableFlowConvertedToNativeFlow(CurrentSubreach)= &
                    SubreachSumReusableFlowConvertedToNativeFlow(CurrentSubreach)+ &
                         RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRFAtNode)
               
               !Set upstream flow value for entity to 0--no further t/l calculations.
               if (ConvergenceFlag) UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFAtNode)=0.0

          !If delivery node is greater than input node (downstream delivery), set delivery flow equal 
          !to current flow value for specific entity.
          else if (ReusableReturnFlowDeliveryNode(CurrentRRFAtNode) > ReusableReturnFlowReleaseNode(CurrentRRFAtNode)) then
               
               RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRFAtNode) = &
                    UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFAtNode)
               
               !Subtract delivery discharge from subreach beginning reusable flow.
               if (CurrentRRFAtNode >= 9) TotalRRFForCurrentSubreach=TotalRRFForCurrentSubreach- &
                    RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRFAtNode)
               
               !Set value of reusable flow converted to native flow.
               if (CurrentRRFAtNode >= 9) SubreachSumReusableFlowConvertedToNativeFlow(CurrentSubreach)= &
                    SubreachSumReusableFlowConvertedToNativeFlow(CurrentSubreach)+ &
                         RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRFAtNode)

               !Set upstream flow value for entity to 0--no further t/l calculations.
               if (ConvergenceFlag) UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFAtNode)=0.0
          end if CheckForDelveryNodeSameAsInputNode
          
     end if CheckForRRFDeliveryNodeLocatedOnCurrentSubreach
     
end do LoopThroughAllRRFsInSubreachAndEstimateDeliveryFlow

!Call subroutine to evaluate transmountain diversions.
if (CurrentSubreach >= NumberOfMonumentCreekSubreaches+1) then
     if (present(PerformTransitLossSavingsComputation)) then
          call CheckIfCSDiversionsLessThanRRF (TotalRRFForCurrentSubreach,CurrentSubreach,PerformTransitLossSavingsComputation)
     else
          call CheckIfCSDiversionsLessThanRRF (TotalRRFForCurrentSubreach,CurrentSubreach)
     endif
endif

!Finally, set new percentages of reusable return flows for each return-flow entity at beginning of current subreach. These
!percentages then are used to calculate estimated transit losses for each individual reusable return flow in the "EstimateMonumentCreekRRFTransitLosses" 
!and "EstimateFountainCreekRRFTransitLosses" subroutines.

LoopThroughAllRRFsInSubreachAndEstimatePercentOfTotalRRF: do CurrentRRFAtNode=1,NumberOfReusableReturnFlows
     CheckForSubreachRRFAndTotalRRFGTZero: if (TotalRRFForCurrentSubreach > 0.0 .and. &
          UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFAtNode) >= 0.0) then
               FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRFAtNode)= &
                    UpstreamReusableReturnFlowByRRFEntity(CurrentSubreach,CurrentRRFAtNode)/TotalRRFForCurrentSubreach*100.
     else
          FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRFAtNode)=0.0
     end if CheckForSubreachRRFAndTotalRRFGTZero
end do LoopThroughAllRRFsInSubreachAndEstimatePercentOfTotalRRF

return

end

!*****************************************************************************
subroutine CheckIfCSDiversionsLessThanRRF (TotalRRFInSubreach,FountainCreekSubreach,PerformTransitLossSavingsComputation)
!*****************************************************************************
!Subroutine checks if Colorado Springs transmountain diversions are less than transmountain return flows 
!and writes comment for use on output report.

!Definition of local variables:
!-----------------------------------------------------------------------------
!TotalRRFInSubreach = local variable for the total reusable return flow, carried from calling subroutine, 
!modified here, and then carried back.
!FountainCreekSubreachNumber = compute value for number of subreach along Fountain Creek.
!FountainCreekSubreach = local variable for the number of the current subreach, from calling subroutine.
!*****************************************************************************
use CommonVariables, &
     only : ConvergenceFlag,FlagForExchangeDiversion,FlagForFryArkDiversion,FlagForRRFDiversion, &
     NumberOfMonumentCreekSubreaches, SubreachSumFryArkDiversionsAlongFountainCreek,UpstreamReusableReturnFlowByRRFEntity, &
     SubreachSumFryArkDiversionsAlongFountainCreek,SubreachSumFryArkDiversionsAlongFountainCreek, &
     SubreachSumExchangeDiversionsAlongFountainCreek,SubreachTransitLossFromFountainCreekDiversion, &
     SubreachSumReusedDiversionsAlongFountainCreek,SCRATCH2FileUnit
implicit none

real :: TotalRRFInSubreach,AddTLSDiversion
integer :: FountainCreekSubreach,FountainCreekSubreachNumber
logical, intent(in), optional :: PerformTransitLossSavingsComputation

!If there are any transmountain diversions at upstream node of current subreach, evaluate diversion 
!quantity and adjust appropriate variables.
!Fry-Ark diversions.
FountainCreekSubreachNumber=FountainCreekSubreach-NumberOfMonumentCreekSubreaches
CheckForFryArkDiversionWithNonZeroFountainDiversion: if (FlagForFryArkDiversion .and. &
     SubreachSumFryArkDiversionsAlongFountainCreek(FountainCreekSubreachNumber) > 0.0) then
     !Write comment for output if TM diversion greater than TM flow.
     ! Not used when computing TL savings.
     if (.not. present(PerformTransitLossSavingsComputation)) then
          if (UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,6) < &
               SubreachSumFryArkDiversionsAlongFountainCreek(FountainCreekSubreachNumber) .and. ConvergenceFlag) &
               write (SCRATCH2FileUnit,1000) SubreachSumFryArkDiversionsAlongFountainCreek(FountainCreekSubreachNumber), &
               UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,6),FountainCreekSubreach
     end if
     
     CheckForConvergenceAndSubtractDiversionFromReturnFlow: if (ConvergenceFlag) then
          UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,6)= &
               UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,6)- &
               SubreachSumFryArkDiversionsAlongFountainCreek(FountainCreekSubreachNumber)
          TotalRRFInSubreach=TotalRRFInSubreach-SubreachSumFryArkDiversionsAlongFountainCreek(FountainCreekSubreachNumber)
     end if CheckForConvergenceAndSubtractDiversionFromReturnFlow
end if CheckForFryArkDiversionWithNonZeroFountainDiversion

!Exchange and CSU reusable diversions. If both exchange and reusable 
!diversions in use and sum is greater than CSU return flow, first set
!reusable diversion to zero, then recheck using just exchange diversion. 
!Check exchange diversions.
CheckForReusableNonZeroExchangeFlow: if (FlagForRRFDiversion .and. FlagForExchangeDiversion .and. &
     SubreachSumExchangeDiversionsAlongFountainCreek(FountainCreekSubreachNumber) > 0.0) then
     !Write comment for output if TM diversion greater than TM flow.
     
     ! Not used when computing TL savings.
     if (.not. present(PerformTransitLossSavingsComputation)) then
          if (UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,5) < &
               SubreachSumExchangeDiversionsAlongFountainCreek(FountainCreekSubreachNumber) .and. ConvergenceFlag) &
               write (SCRATCH2FileUnit,1020) SubreachSumExchangeDiversionsAlongFountainCreek(FountainCreekSubreachNumber), &
                    UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,5),FountainCreekSubreach
     end if
     CheckForConvergenceAndSubtractExchangableDiversionFromRRF: if (ConvergenceFlag) then
          AddTLSDiversion=SubreachTransitLossFromFountainCreekDiversion(FountainCreekSubreachNumber)
          ! Not used when computing TL savings.  
          if (.not. present(PerformTransitLossSavingsComputation)) AddTLSDiversion=0.0
          UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,5)= &
               UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,5)- &
               SubreachSumExchangeDiversionsAlongFountainCreek(FountainCreekSubreachNumber)+AddTLSDiversion
          TotalRRFInSubreach=TotalRRFInSubreach-SubreachSumExchangeDiversionsAlongFountainCreek(FountainCreekSubreachNumber)+ &
               AddTLSDiversion
     end if CheckForConvergenceAndSubtractExchangableDiversionFromRRF
     return
end if CheckForReusableNonZeroExchangeFlow

!Reuse diversion. 
!Assumes that exchange diversions have higher priority over reusable diversions.
CheckIfReusableDiversion: if (FlagForRRFDiversion .and. .not. FlagForExchangeDiversion .and. &
     SubreachSumReusedDiversionsAlongFountainCreek(FountainCreekSubreachNumber) > 0.0) then
     !Write comment for output if TM diversion greater than TM flow.
     ! Not used when computing TL savings.
     if (.not. present(PerformTransitLossSavingsComputation)) then
          if (UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,5) < &
               SubreachSumReusedDiversionsAlongFountainCreek(FountainCreekSubreachNumber) .and. ConvergenceFlag) &
               write (SCRATCH2FileUnit,1030) SubreachSumReusedDiversionsAlongFountainCreek(FountainCreekSubreachNumber), &
               UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,5),FountainCreekSubreach
     end if
     CheckForConvergenceAndSubtractReusableDiversionFromRRF: if (ConvergenceFlag) then
          UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,5)= &
               UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,5)- &
               SubreachSumReusedDiversionsAlongFountainCreek(FountainCreekSubreachNumber)
          TotalRRFInSubreach=TotalRRFInSubreach-SubreachSumReusedDiversionsAlongFountainCreek(FountainCreekSubreachNumber)
     end if CheckForConvergenceAndSubtractReusableDiversionFromRRF
end if CheckIfReusableDiversion

return

1000 format ('ITCHK CSU Fry-Ark diversions (',f6.2,') larger than Fry-Ark flows (',f6.2, ') in subreach ',i2,'.')
1010 format ('ITCHK CSU exch. and reuse diversions (',f6.2,') larger than transmountain flows (',f6.2, ') in subreach ',i2,'.')
1020 format ('ITCHK CSU exchange diversions (',f6.2,') larger than transmountain flows (',f6.2, ') in subreach ',i2,'.')
1030 format ('ITCHK CSU reuse diversions (',f6.2,') larger than transmountain flows (',f6.2,') in subreach ',i2,'.')

end

!*****************************************************************************
subroutine InterpolateBankStorageLoss (TemporaryBankStorageLoss,CurrentSubreachNumber)
!*****************************************************************************
!Subroutine searches the reach specific initial bank storage array and interpolates bank storage 
!loss based on reusable return flow (TemporarySubreachUpstreamTotalRRF2)
!and native flow (TemporarySubreachUpstreamNativeFlow2) for specified reach (CurrentSubreachNumber).

!Definition of local variables:
!-----------------------------------------------------------------------------
!CurrentSubreachNumber = current subreach number.
!NativeFlowColumn = values for native streamflow columns in "RRFBankStorageLossLookupTableForBothCreeks," (in cfs).
!RoundedRRF = local integer value for reusable return flow.
!RoundedNativeFlow = local integer value for native flow.
!TemporaryBankStorageLoss = value for bank storage loss, carried to calling subroutine for current values of reusable and native flow.
!TemporarySubreachUpstreamTotalRRF2 = real value for reusable return flow, from calling subroutine
!TemporarySubreachUpstreamNativeFlow2 = real value for native flow, from calling subroutine.
!SlopeOfInterpolationLine = difference between the native flows in the current row of RRFBankStorageLossLookupTableForBothCreeks 
!that bracket the value for current native flow (RoundedNativeFlow), 
!        or SlopeOfInterpolationLine of line interpolation line.
!InterceptOfInterpolationLine = intercept of interpolation line.
!*****************************************************************************
use CommonVariables, &
     only : TemporarySubreachUpstreamNativeFlow2,TemporarySubreachUpstreamTotalRRF2, &
     RRFBankStorageLossLookupTableForBothCreeks
implicit none

real, intent(inout) :: TemporaryBankStorageLoss
real :: SlopeOfInterpolationLine,InterceptOfInterpolationLine
integer, intent(in) :: CurrentSubreachNumber
integer :: DiscreteNativeFlowsInTable(16),RoundedNativeFlow,RoundedRRF,FoundRow,FoundColumn,CurrentColumn


data DiscreteNativeFlowsInTable/0,1,2,5,10,20,30,40,50,75,100,200,300,500,700,1000/

!Set integer value for reusable and native flows.
RoundedRRF=nint(TemporarySubreachUpstreamTotalRRF2)
RoundedNativeFlow=nint(TemporarySubreachUpstreamNativeFlow2)

!Set row indicator for bank storage array.
FoundRow=(CurrentSubreachNumber-1)*200+RoundedRRF

!If native flow >= 1000, set bank storage loss to last table value.
CheckForNativeFlowExceeding1000CFS: if (RoundedNativeFlow >= 1000) then
     TemporaryBankStorageLoss=RRFBankStorageLossLookupTableForBothCreeks(FoundRow,16)
     return
end if CheckForNativeFlowExceeding1000CFS

!Locate range for column indicator in bank-storage loss array (RRFBankStorageLossLookupTableForBothCreeks). 
!Column values based on integer of native flow and next DiscreteNativeFlowsInTable value.
!Locate column that is greater than or equal to current native flow.
FoundColumn=0
LoopThroughLookupTableToFindNativeFlowColumn: do CurrentColumn=2,16
     FoundColumn=CurrentColumn
     if (RoundedNativeFlow >= DiscreteNativeFlowsInTable(CurrentColumn-1) .and. &
          RoundedNativeFlow < DiscreteNativeFlowsInTable(CurrentColumn)) &
          goto 20
end do LoopThroughLookupTableToFindNativeFlowColumn

!Here we go for the interpolation.
!  SlopeOfInterpolationLine=(y-y)/(x-x)
!  intercept=y-SlopeOfInterpolationLine*x
!  y=SlopeOfInterpolationLine*x+intercept

20 SlopeOfInterpolationLine = (RRFBankStorageLossLookupTableForBothCreeks(FoundRow,FoundColumn)- &
     RRFBankStorageLossLookupTableForBothCreeks(FoundRow,FoundColumn-1)) / &
     (float(DiscreteNativeFlowsInTable(FoundColumn)-DiscreteNativeFlowsInTable(FoundColumn-1)))
InterceptOfInterpolationLine=RRFBankStorageLossLookupTableForBothCreeks(FoundRow,FoundColumn)- &
     SlopeOfInterpolationLine*float(DiscreteNativeFlowsInTable(FoundColumn))
TemporaryBankStorageLoss=SlopeOfInterpolationLine*TemporarySubreachUpstreamNativeFlow2+InterceptOfInterpolationLine

return

end

!*****************************************************************************
subroutine InterpolateBankStorageLossForLowMonumentRRF (TemporaryBankStorageLoss,CurrentSubreachNumber)
!*****************************************************************************
!New subroutine to derive initial bank storage loss (look-up table) for Monument Creek for reusable return flows less than 4.75 cfs. G Kuhn, 03/2012.
!Subroutine searches the reach specific initial bank storage array and interpolates bank storage loss based on reusable return flow (TemporarySubreachUpstreamTotalRRF2)
!and native flow (TemporarySubreachUpstreamNativeFlow2) for specified reach (CurrentSubreachNumber).
!Definition of local variables:
!-----------------------------------------------------------------------------
!CurrentSubreachNumber = current subreach number.
!RoundedRRF = local value for reusable return flow.
!RoundedNativeFlow = local integer value for native flow.
!TemporaryBankStorageLoss = value for bank storage loss, carried to calling subroutine for current values of reusable and native flow.
!TemporarySubreachUpstreamTotalRRF2 = real value for reusable return flow, from calling subroutine
!TemporarySubreachUpstreamNativeFlow2 = real value for native flow, from calling subroutine.
!SlopeOfInterpolationLine = difference between the native flows in the current row of RRFBankStorageLossLookupTableForBothCreeks that bracket the value for current native flow (RoundedNativeFlow), or
!InterceptOfInterpolationLine = intercept of interpolation line.
!*****************************************************************************
use CommonVariables, &
     only : TemporarySubreachUpstreamNativeFlow2,TemporarySubreachUpstreamTotalRRF2, &
     LowerRangeOfIntervalsForRRFBankStorageLossLookupTableEntry,UpperRangeOfIntervalsForRRFBankStorageLossLookupTableEntry, &
     RRFBankStorageLossLookupTableMonumentCreekLowRRF              
implicit none

real, intent(inout) :: TemporaryBankStorageLoss
integer, intent(in) :: CurrentSubreachNumber
integer :: DiscreteNativeFlowsInTable(16),RoundedNativeFlow,FirstRowInTableForCurrentSubreach, &
     CurrentRow,FoundRow,FoundColumn,CurrentColumn
real :: SlopeOfInterpolationLine,InterceptOfInterpolationLine

integer :: i

data DiscreteNativeFlowsInTable/0,1,2,5,10,20,30,40,50,75,100,200,300,500,700,1000/

!Set integer value for native flow. no integer value for reusable flow.
RoundedNativeFlow=nint(TemporarySubreachUpstreamNativeFlow2)

!First determine subreach location in array (FoundRow).
CheckForRowOfLookupTableCorrespondingtoSubreach: if (CurrentSubreachNumber >= 2) then
     FirstRowInTableForCurrentSubreach=(CurrentSubreachNumber-1)*26+1
else
     FirstRowInTableForCurrentSubreach=1
end if CheckForRowOfLookupTableCorrespondingtoSubreach

!Set row indicator for bank storage array.
LoopThroughRowsToFindRowForRRFAtUpstreamEndOfSubreach: &
     do CurrentRow=FirstRowInTableForCurrentSubreach,FirstRowInTableForCurrentSubreach+25
     if (TemporarySubreachUpstreamTotalRRF2 >= LowerRangeOfIntervalsForRRFBankStorageLossLookupTableEntry(CurrentRow) .and. &
           TemporarySubreachUpstreamTotalRRF2 < UpperRangeOfIntervalsForRRFBankStorageLossLookupTableEntry(CurrentRow)) then
          FoundRow=CurrentRow
          exit
     else
          cycle
     end if
end do LoopThroughRowsToFindRowForRRFAtUpstreamEndOfSubreach

!Locate range for column indicator in bank-storage loss array (RRFBankStorageLossLookupTableForBothCreeks. Column values based on integer of native flow and next DiscreteNativeFlowsInTable value.
!Locate column that is greater than or equal to current native flow.
FoundColumn=0
LoopThroughColumnsToFindRowForRRFAtUpstreamEndOfSubreach: do CurrentColumn=2,16
     FoundColumn=CurrentColumn
     if (RoundedNativeFlow >= DiscreteNativeFlowsInTable(CurrentColumn-1) .and. &
          RoundedNativeFlow < DiscreteNativeFlowsInTable(CurrentColumn)) exit
end do LoopThroughColumnsToFindRowForRRFAtUpstreamEndOfSubreach

!Here we go for the interpolation.
!  SlopeOfInterpolationLine=(y-y)/(x-x)
!  intercept=y-SlopeOfInterpolationLine*x
!  y=SlopeOfInterpolationLine*x+intercept
SlopeOfInterpolationLine = (RRFBankStorageLossLookupTableMonumentCreekLowRRF(FoundRow,FoundColumn) - &
     RRFBankStorageLossLookupTableMonumentCreekLowRRF(FoundRow,FoundColumn-1)) / &
     (float(DiscreteNativeFlowsInTable(FoundColumn)-DiscreteNativeFlowsInTable(FoundColumn-1)))
InterceptOfInterpolationLine=RRFBankStorageLossLookupTableMonumentCreekLowRRF(FoundRow,FoundColumn)- &
     SlopeOfInterpolationLine*float(DiscreteNativeFlowsInTable(FoundColumn))
TemporaryBankStorageLoss=SlopeOfInterpolationLine*TemporarySubreachUpstreamNativeFlow2+InterceptOfInterpolationLine

return

end

!*****************************************************************************
subroutine AdjustBankStorageLossForNonuniformNativeFlow (CurrentSubreachNumber)
!*****************************************************************************
!Subroutine adjusts bank storage loss on basis of ratio between downstream and upstream native streamflow in a subreach.

!Definition of local variables:
!-----------------------------------------------------------------------------
!CurrentSubreachNumber = current subreach number.
!FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow = computed value of adjustment factor, carried to calling subroutine.
!TemporarySubreachDownstreamNativeFlow2 = downstream native flow, from calling subroutine.
!TemporarySubreachUpstreamNativeFlow2 = upstream native flow, from calling subroutine.
!MonumentCreekNonuniformNativeFlowExponentialAdjustmentFactor = array of exponent values for Monument Creek used to calculate adjustment factor on basis of ratio of downstream and upstream native flows.
!FountainCreekNonuniformNativeFlowExponentialAdjustmentFactor = array of exponent values  for fountain creek used to calculate adjustment factor on basis of ratio of downstream and upstream native flows.
!*****************************************************************************
use CommonVariables, &
     only : FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow,NameOfCreek, &
     TemporarySubreachDownstreamNativeFlow2,TemporarySubreachUpstreamNativeFlow2
implicit none

integer, intent(in) :: CurrentSubreachNumber
real :: MonumentCreekNonuniformNativeFlowExponentialAdjustmentFactor(2,14), &
     FountainCreekNonuniformNativeFlowExponentialAdjustmentFactor(2,19)

!Exponents for computing adjustment factors. One set of exponents for us/ds ratios less than one and one set for ratios greater than one.
data MonumentCreekNonuniformNativeFlowExponentialAdjustmentFactor /-.14, -.14, -.15, -.15, -.15, -.16, -.18, -.20, -.23, -.20, &
     -.16, -.22, -.22, -.23, -.15, -.12, -.16, -.13, -.18, -.16, -.12,-.09, -.16, -.21, -.19, -.19, -.22, -.16/
data FountainCreekNonuniformNativeFlowExponentialAdjustmentFactor /-.17, -.16, -.13, -.16, -.13, -.16, -.05, -.03,-.24, -.20, &
     -.13, -.19, -.11, -.21, -.11, -.21, -.08, -.16, -.08, -.16, -.15, -.27, -.12, -.22,-.12, -.22, -.12, -.17, -.11, &
     -.22, -.05, -.15, -.21, -.25, -.12, -.12, -.14, -.29/

CheckForMonthOfCurrentReleaseDateumentOrFountainCreekSubreach: if (NameOfCreek == 'MON') then
     FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=TemporarySubreachDownstreamNativeFlow2/ &
          TemporarySubreachUpstreamNativeFlow2
     CheckForMonthOfCurrentReleaseDateumentCreekAdjustmentFactorGT1: if &
          (FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow > 1) then
          FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow= &
               FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow** &
                    (MonumentCreekNonuniformNativeFlowExponentialAdjustmentFactor(1,CurrentSubreachNumber))
     else if (FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow < 1) then
          FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow= &
               FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow** &
                    (MonumentCreekNonuniformNativeFlowExponentialAdjustmentFactor(2,CurrentSubreachNumber))
     else
          FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=1.0
     end if CheckForMonthOfCurrentReleaseDateumentCreekAdjustmentFactorGT1
else if (NameOfCreek == 'FTN') then
     FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=TemporarySubreachDownstreamNativeFlow2/ &
          TemporarySubreachUpstreamNativeFlow2
     CheckForFountainCreekAdjustmentFactorGT1: if (FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow > 1) then
          FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow= &
               FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow** &
                    (FountainCreekNonuniformNativeFlowExponentialAdjustmentFactor(1,CurrentSubreachNumber))
     else if (FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow < 1) then
          FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow= &
               FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow** &
                    (FountainCreekNonuniformNativeFlowExponentialAdjustmentFactor(2,CurrentSubreachNumber))
     else
          FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=1.0
     end if CheckForFountainCreekAdjustmentFactorGT1
end if CheckForMonthOfCurrentReleaseDateumentOrFountainCreekSubreach

return

end

!*****************************************************************************
subroutine ComputeBankStorageRecovery (BankStorageLoss,BankStorageGain,CurrentSubreach,CurrentRRF)
!*****************************************************************************
!Subroutine computes bank storage gains for each day in the recovery period, which varies from 1 to 12 days for monument creek subreaches and 
!is 60 days for all Fountain Creek subreaches, except for first subreach, which has 28 day recovery period.

!Definition of local variables:
!-----------------------------------------------------------------------------
!LengthOfBankStorageRecoveryPeriod = local variable for length of recovery period, from calling subroutine.
!PreviousGainsFromBankStorage = array of previous values of gains from bank storage.
!BankStorageLoss = local variable for bank storage loss, carried from calling subr.
!BankStorageGain = locally computed value of gains from bank storage for current day and subreach and return flow (irt)
!PercentBankStorageLossRemainingInStorageMonumentCreek = array of percentages of bank storage loss remaining in bank storage on any given day of recovery period for each subreach
!     along Monument Creek.
!PercentBankStorageLossRemainingInStorageFountainCreek = array of percentages of bank storage loss remaining in bank storage on any given day of recovery period for each subreach
!     along Fountain Creek.
!FractionBankStorageHeld,FractionBankStorageReleased,PreviousFractionBankStorageHeld = variables to compute gains from bank storage. For Monument Creek, the daily percentages are specified
!     for each subreach in the PercentBankStorageLossRemainingInStorageMonumentCreek array. For Fountain Creek, values for subreach 1 are in the PercentBankStorageLossRemainingInStorageFountainCreek
!     array, and specified by the variables for a nonlinear regression equation.
!*****************************************************************************
use CommonVariables, &
     only : ConvergenceFlag,FlagForScratchOutput,SCRATCH1FileUnit,SubreachBankStorageReleasedDuringRecovery, &
     SubreachBankStorageRecoveryPeriodLengthInDays
implicit none

real, intent(in) :: BankStorageLoss
real, intent(inout) :: BankStorageGain
integer, intent(in) :: CurrentSubreach,CurrentRRF
integer :: LengthOfBankStorageRecoveryPeriod,CurrentNativeFlow
real :: PercentBankStorageLossRemainingInStorageMonumentCreek(12,14), &
     PercentBankStorageLossRemainingInStorageFountainCreek(28), &
     PreviousGainsFromBankStorage(61),FractionBankStorageHeld, &
     FractionBankStorageReleased,PreviousFractionBankStorageHeld

data PercentBankStorageLossRemainingInStorageMonumentCreek /48.96, 23.83, 12.68, 6.13, 8*0.0, 64.09, 22.80, 4.72, 9*0.0, 91.60, &
     11*0.0, 91.60, 11*0.0, 89.91, 1.69, 10*0.0, 90.66, 0.94, 10*0.0, 91.60, 11*0.0, 91.60, 11*0.0, 91.60, 11*0.0, 80.46, 11.14, & 
     10*0.0, 90.32, 1.28, 10*0.0, 91.60, 11*0.0,41.63, 13.41, 7.97, 6.03, 4.91, 4.11, 3.47, 2.91, 2.45, 2.06, 1.75, 0.92, 41.20, &
     15.75, 10.62, 7.93, 5.99, 4.53, 3.42, 2.17,4*0.0/
data PercentBankStorageLossRemainingInStorageFountainCreek /41.70, 13.13, 6.97, 4.50, 3.22, 2.46, 1.99, 1.68, 1.47, 1.32, 1.21, &
     1.09, 1.00, 0.96, 0.90, 0.86, 0.78, 0.77, 0.72,0.65, 0.65, 0.62, 0.59, 0.56, 0.51, 0.50, 0.47, 0.32/


!Reinitialize the "PreviousGainsFromBankStorage" array and set new values from the "SubreachBankStorageReleasedDuringRecovery" array that was read in the "ReadBankStorageLossRecoveryTable" subroutine.

PreviousGainsFromBankStorage=0.0


LengthOfBankStorageRecoveryPeriod=SubreachBankStorageRecoveryPeriodLengthInDays(CurrentSubreach)


BankStorageGain=SubreachBankStorageReleasedDuringRecovery(CurrentSubreach,CurrentRRF,2)

LoopThroughTableAndEnterBankStorageHeldFromPreviousDay: do CurrentNativeFlow=1,LengthOfBankStorageRecoveryPeriod-1
     !Skip the first two values (CurrentNativeFlow+2) per the original program.
     PreviousGainsFromBankStorage(CurrentNativeFlow) = &
          SubreachBankStorageReleasedDuringRecovery(CurrentSubreach,CurrentRRF,CurrentNativeFlow+2)
end do LoopThroughTableAndEnterBankStorageHeldFromPreviousDay

!Add new recovery values to array.
CheckForConvergence: if (ConvergenceFlag) then
     PreviousFractionBankStorageHeld=1.0
     LoopThroughAlDayOfLastReleaseDatesInRecoveryPeriod: do CurrentNativeFlow=1,LengthOfBankStorageRecoveryPeriod
          CheckForSubreachLocation: if (CurrentSubreach <= 14) then
                 FractionBankStorageHeld = &
                    PercentBankStorageLossRemainingInStorageMonumentCreek(CurrentNativeFlow,CurrentSubreach)/100.0
          else if (CurrentSubreach == 15) then
                 FractionBankStorageHeld=PercentBankStorageLossRemainingInStorageFountainCreek(CurrentNativeFlow)/100.0
          else if (CurrentSubreach >= 16) then
                 FractionBankStorageHeld=((75.6*CurrentNativeFlow**(-0.348))-9.8)/100.0
          end if CheckForSubreachLocation
          FractionBankStorageReleased=PreviousFractionBankStorageHeld-FractionBankStorageHeld
          PreviousFractionBankStorageHeld=FractionBankStorageHeld
          PreviousGainsFromBankStorage(CurrentNativeFlow) = PreviousGainsFromBankStorage(CurrentNativeFlow) + &
          (BankStorageLoss*FractionBankStorageReleased)
          !To prevent retaining values for gains from bank storage that are extremely small or to correct improper "zeroing" of values (such as when
          !new "recov" is created).
          !if (PreviousGainsFromBankStorage(CurrentNativeFlow) < 0.00000001) PreviousGainsFromBankStorage(CurrentNativeFlow)=0.0
     end do LoopThroughAlDayOfLastReleaseDatesInRecoveryPeriod

     !If US and Downstream flows have converged, write to scratch file for later update of bank storage recovery file (cs_recov.fil)
     if (FlagForScratchOutput) then
          write (SCRATCH1FileUnit,*) &
               BankStorageGain,(PreviousGainsFromBankStorage(CurrentNativeFlow), &
               CurrentNativeFlow=1,LengthOfBankStorageRecoveryPeriod)
     end if 

end if CheckForConvergence

return

end
!*****************************************************************************
subroutine ComputeSubreachChannelStorageLoss (CurrentSubreachNumber)
!*****************************************************************************
!Subroutine computes channel storage loss on basis of average reusable flow in subreach, retrieves loss from yesterday, and sends values back to
!calling subroutine. Channel loss is calculated on basis of percentage values for subreaches determined in stream-aquifer model applications.

!Definition of local variables:
!-----------------------------------------------------------------------------
!CurrentSubreachNumber = current subreach number.
!SubreachChannelStorageLoss = local variable for value of channel storage loss.
!SubreachChannelStorageGain = local variable for value of channel storage gain.
!TemporarySubreachUpstreamTotalRRF2 = local variable for value of upstream reusable flow.
!TemporarySubreachDownstreamTotalRRF2 = local variable for value of downstream reusable flow.
!*****************************************************************************
use CommonVariables, &
     only : ConvergenceFlag,FlagForScratchOutput,NameOfCreek,SubreachChannelStorageGain, &
     SubreachChannelStorageLoss,TemporarySubreachUpstreamTotalRRF2,SubreachChannelStorageLossAlongMonumentCreek, &
     SubreachChannelStorageLossAlongFountainCreek,TemporarySubreachDownstreamTotalRRF2,SCRATCH1FileUnit
implicit none

integer, intent(in) :: CurrentSubreachNumber

!Compute average reusable flow. Store today's bank loss and move yesterday's loss up. Today's loss is tomorrow's gain.
!   Monument Creek
CheckMonumentorFountain: if (NameOfCreek == 'MON') then   
     SubreachChannelStorageLoss=(TemporarySubreachUpstreamTotalRRF2+TemporarySubreachDownstreamTotalRRF2)/2.
     CheckForMonthOfCurrentReleaseDateumentReachLocation: if (CurrentSubreachNumber == 9) then
          SubreachChannelStorageLoss=SubreachChannelStorageLoss*0.20
     else if (CurrentSubreachNumber == 12) then
          SubreachChannelStorageLoss=SubreachChannelStorageLoss*0.20
     else if (CurrentSubreachNumber == 7) then
          SubreachChannelStorageLoss=SubreachChannelStorageLoss*0.30
     else
          SubreachChannelStorageLoss=SubreachChannelStorageLoss*0.10
     end if CheckForMonthOfCurrentReleaseDateumentReachLocation
     if (SubreachChannelStorageLoss < 0.0) SubreachChannelStorageLoss=0.0

     !Write to scratch file for later update of recovery file.
     SubreachChannelStorageGain=SubreachChannelStorageLossAlongMonumentCreek(CurrentSubreachNumber)
     if (ConvergenceFlag) write (SCRATCH1FileUnit,*) SubreachChannelStorageGain,SubreachChannelStorageLoss

!Compute average reusable flow. Store today's bank loss and move yesterday's loss up. Today's loss is tomorrow's gain.
!  Fountain Creek 
else if (NameOfCreek == 'FTN') then     
     SubreachChannelStorageLoss=(TemporarySubreachUpstreamTotalRRF2+TemporarySubreachDownstreamTotalRRF2)/2.
     CheckForFountainReachLocation: if (CurrentSubreachNumber == 18) then
          SubreachChannelStorageLoss=SubreachChannelStorageLoss*0.20
     else
          SubreachChannelStorageLoss=SubreachChannelStorageLoss*0.10
     end if CheckForFountainReachLocation
     if (SubreachChannelStorageLoss < 0.0) SubreachChannelStorageLoss=0.0

     !Write to scratch file for later update of recovery file.
     SubreachChannelStorageGain=SubreachChannelStorageLossAlongFountainCreek(CurrentSubreachNumber)
     CheckIfWriteToScratchFile: if (FlagForScratchOutput) then
          if (ConvergenceFlag) write (SCRATCH1FileUnit,*) SubreachChannelStorageGain,SubreachChannelStorageLoss
     end if CheckIfWriteToScratchFile
end if CheckMonumentorFountain

return

end

!*****************************************************************************
subroutine CalculateRRFEvaporativeLoss (TemporaryEvaporationLoss,CurrentSubreachChannelLength)
!*****************************************************************************
!Subroutine calculates evaporative loss for reusable return flows.

!Definition of local variables:
!-----------------------------------------------------------------------------
!MonthlyEvaporationRateMonumentCreek = array of monthly evaporation rates for Monument Creek.
!MonthlyEvaporationRateFountainCreek = array of monthly evaporation rates for Fountain Creek.
!CurrentSubreachChannelLength = length of channel for current subreach.
!IncrementalStreamWidthIncrease = calculated incremental increase in stream width resulting from reusable return flow in stream. stream widths are calculated
!        from regression equations presented in stream-aquifer modeling reports cited a beginning of source code.
!TemporarySubreachUpstreamNativeFlow2 = upstream native flow in subreach, from calling subroutine.
!TemporarySubreachDownstreamNativeFlow2 = downstream native flow in subreach, from calling subroutine.
!*****************************************************************************
use CommonVariables, &
     only : FountainCreekStreamWidthVsFlowExponent,FountainCreekStreamWidthVsFlowScalingFactor, &
     MonumentCreekStreamWidthVsFlowExponent,MonumentCreekStreamWidthVsFlowScalingFactor,MonthOfCurrentReleaseDate, &
     NameOfCreek,TemporarySubreachUpstreamNativeFlow2,TemporarySubreachDownstreamNativeFlow2, &
     TemporarySubreachUpstreamTotalRRF2,TemporarySubreachDownstreamTotalRRF2
implicit none

real, intent(inout) :: TemporaryEvaporationLoss
real, intent(in) :: CurrentSubreachChannelLength
real :: MonthlyEvaporationRateMonumentCreek(12),MonthlyEvaporationRateFountainCreek(12), &
     IncrementalStreamWidthIncrease,AverageNativeFlow,AverageRRF,StreamWidthUnderNativeFlowPlusRRF, &
     StreamWidthUnderNativeFlow

data MonthlyEvaporationRateMonumentCreek /.000284, .000327, .000449, .000721, .000934, .001149, .001124, .001017, .000819, &
     .000603, .000367, .000284/
data MonthlyEvaporationRateFountainCreek /.000278, .000406, .000648, .000898, .000979, .001290, .001290, .000979, .000898, &
     .000648, .000406, .000278/

!Calculate change in stream width due to reusable return flow for Monument Creek.
CheckForMonthOfCurrentReleaseDateumentOrFountainCreekSubreach: if (NameOfCreek == 'MON') then
     AverageNativeFlow=(TemporarySubreachUpstreamNativeFlow2+TemporarySubreachDownstreamNativeFlow2)/2.
     AverageRRF=(TemporarySubreachUpstreamTotalRRF2+TemporarySubreachDownstreamTotalRRF2)/2.
     StreamWidthUnderNativeFlowPlusRRF=FountainCreekStreamWidthVsFlowScalingFactor*(AverageRRF+AverageNativeFlow)** &
          FountainCreekStreamWidthVsFlowExponent
     StreamWidthUnderNativeFlow=FountainCreekStreamWidthVsFlowScalingFactor*AverageNativeFlow** &
          FountainCreekStreamWidthVsFlowExponent
     IncrementalStreamWidthIncrease=StreamWidthUnderNativeFlowPlusRRF-StreamWidthUnderNativeFlow
     if (IncrementalStreamWidthIncrease < 0.0) IncrementalStreamWidthIncrease=0.0

     !Calculate evaporative loss for indicated month and width increase.
     TemporaryEvaporationLoss=MonthlyEvaporationRateMonumentCreek(MonthOfCurrentReleaseDate)*CurrentSubreachChannelLength* &
          IncrementalStreamWidthIncrease

!Calculate change in stream width due to reusable return flow for Fountain Creek.
else if (NameOfCreek == 'FTN') then
     AverageNativeFlow=(TemporarySubreachUpstreamNativeFlow2+TemporarySubreachDownstreamNativeFlow2)/2.
     AverageRRF=(TemporarySubreachUpstreamTotalRRF2+TemporarySubreachDownstreamTotalRRF2)/2.
     StreamWidthUnderNativeFlowPlusRRF=MonumentCreekStreamWidthVsFlowScalingFactor*(AverageRRF+AverageNativeFlow)** &
          MonumentCreekStreamWidthVsFlowExponent
     StreamWidthUnderNativeFlow=MonumentCreekStreamWidthVsFlowScalingFactor*AverageNativeFlow** &
          MonumentCreekStreamWidthVsFlowExponent
     IncrementalStreamWidthIncrease=StreamWidthUnderNativeFlowPlusRRF-StreamWidthUnderNativeFlow
     if (IncrementalStreamWidthIncrease < 0.0) IncrementalStreamWidthIncrease=0.0

     !Calculate evaporative loss for indicated month and width increase.
     TemporaryEvaporationLoss=MonthlyEvaporationRateFountainCreek(MonthOfCurrentReleaseDate)*CurrentSubreachChannelLength* &
          IncrementalStreamWidthIncrease
     
end if CheckForMonthOfCurrentReleaseDateumentOrFountainCreekSubreach

return

end

!*****************************************************************************
subroutine EstimateFountainCreekRRFTransitLossesWithExchange (NetRRFQInMonumentDischargedToFountain, &
     NetNativeQInMonumentDischargedToFountain,InputErrorFlag)
!*****************************************************************************
!Duplicate subroutine of "EstimateFountainCreekRRFTransitLosses," but this version is called only when any of 8 SE exchange diversions (see subroutine "ReadDiversionDataInput")
!are specified on input. In this subroutine, any SE exchange diversions for these 8 ditches are not used and are treated as part of any 
!CSU reusable return flows. The result is a different calculation of transit loss to the mouth of Fountain Creek than with the "EstimateFountainCreekRRFTransitLosses" 
!subroutine in which the 8 SE exchange diversions are used. The difference in the two subroutine calculations enables computation of 
!"transit loss savings" for SE exchange diversions. G. Kuhn. Jan, 2010.

!Definition of local variables:
!-----------------------------------------------------------------------------
!LastSubreachInStreamSegment = ending subreach within a stream segment (between gaging stations).
!BeginningSubreachInStreamSegment = beginning subreach within a stream segment (between gaging stations).
!AccumulatedFryArkExchangeAndRRFDiversions = sum of all transmountain diversions in current subreach.
!NumberOfIterationsForConvergence = the number of iterations made for the stream segment and subreach loops before the convergence criterion is met.
!FountainCreekSegment = local variables to indicate number for segment or subreach loops.
!FountainCreekSubreach = local variables to indicate number for segment or subreach loops.
!NetRRFQInMonumentDischargedToFountain = the net (total) amount of reusable return flow at the end of Monument Creek reach (carried from subroutine EstimateFountainCreekRRFTransitLosses).
!NetNativeQInMonumentDischargedToFountain = the net (total) amount of native flow at the end of Monument Creek reach (carried from subroutine EstimateFountainCreekRRFTransitLosses).
!*****************************************************************************
use CommonVariables, &
     only : ConvergenceFlag,CSURRFDeliveryWithoutTransitLossSavings,FlagForScratchOutput, &
     FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow,FlagForSubreachOrSegmentLevel, &
     NameOfCreek,NumberOfFountainCreekSegments,NumberOfMonumentCreekSegments,NumberOfReusableReturnFlowsEntered, &
     PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest,SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow, &
     SegmentDifferenceComputedDownstreamQCurrentAndLastIteration,SegmentDownstreamTotalGagedFlow,SegmentDownstreamTotalNativeFlow,&
     SegmentGagedMainstemFlow,SegmentGagedTributaryFlow,SegmentGainOrLossInNativeFlowBetweenGagingStations, &
     SegmentNativeFlowGainOrLossPerMile,SegmentUpstreamTotalGagedFlow,SegmentUpstreamTotalNativeFlow, &
     SubreachBankStorageGainMinusLoss,SubreachChannelStorageGain,SubreachChannelStorageLoss,SubreachGagedMainstemFlow, &
     SubreachGagedTributaryFlow,TemporarySubreachDownstreamNativeFlow,TemporarySubreachDownstreamNativeFlow2, &
     TemporarySubreachDownstreamTotalRRF,TemporarySubreachDownstreamTotalRRF2,TemporarySubreachUpstreamNativeFlow, &
     NumberOfMonumentCreekSubreaches,NumberOfFountainCreekSubreaches,TemporarySubreachUpstreamNativeFlow2, &
     TemporarySubreachUpstreamTotalRRF,TemporarySubreachUpstreamTotalRRF2,GagingStationDischarge_Mainstem, &
     TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ,FountainCreekGagingStationNode, &
     SubreachUpstreamTotalRRFAlongFountainCreek,SubreachDownstreamTotalRRFAlongFountainCreek, &
     SegmentTransitLossFromFromFountainCreekDiversion,SegmentSumReusedDiversionsAlongFountainCreek, &
     SegmentSumNativeFlowDiversionsAlongFountainCreek,FountainCreekSegmentLength,SubreachUpstreamNativeFlowAlongFountainCreek, &
     SubreachUpstreamNativeFlowAlongFountainCreek,SubreachUpstreamNativeFlowAlongFountainCreek, &
     SegmentSumExchangeDiversionsAlongFountainCreek,SubreachDownstreamNativeFlowAlongFountainCreek, &
     SubreachSumNativeFlowDiversionsAlongFountainCreek,SegmentSumFryArkDiversionsAlongFountainCreek, &
     SubreachSumReusableFlowConvertedToNativeFlow,SubreachGainOrLossNativeFlowAlongFountainCreek,FountainCreekSubreachLength, &
     SubreachGainOrLossNativeFlowAlongFountainCreek,SubreachBankStorageLossAlongFountainCreek, &
     SubreachBankStorageGainAlongFountainCreek,SubreachBankStorageLossForEachRRFRelease, &
     FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions,SubreachBankStorageGainForEachRRFRelease, &
     SubreachEvaporationLossFountainCreek,SubreachGainOrLossAllReusableFlowAlongFountainCreek, &
     DownstreamReusableReturnFlowByRRFEntity,UpstreamReusableReturnFlowByRRFEntity,DownstreamReusableReturnFlowByRRFEntity, &
     ReusableReturnFlowDeliveryNode,RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions
implicit none

!#Create an interface so that you can pass an optional argument to subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach.
interface
     subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach(TotalRRFForCurrentSubreach,CurrentSubreach, &
          CurrentSegment,NumberOfIterationsForConvergence,PerformTransitLossSavingsComputation)
          integer :: CurrentSubreach,CurrentSegment,NumberOfIterationsForConvergence,CurrentRRFInSubreach,CurrentRRFDiversion, &
	           CurrentRRFEntity,CurrentRRFAtNode,CurrentRRF
	      logical, intent(in), optional :: PerformTransitLossSavingsComputation
	      real :: TotalRRFForCurrentSubreach
     end subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach
end interface

real, intent(in) :: NetRRFQInMonumentDischargedToFountain,NetNativeQInMonumentDischargedToFountain
integer, intent(in) :: InputErrorFlag
integer :: BeginningSubreachInStreamSegment,LastSubreachInStreamSegment,FountainCreekSegment,FountainCreekSubreach, &
     NumberOfIterationsForConvergence,CurrentSegmentNumber,CurrentSubreach,CurrentRRF
real :: AccumulatedFryArkExchangeAndRRFDiversions,TemporaryRRF

     
FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=1.0
SegmentNativeFlowGainOrLossPerMile=0.0
PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest=0.0
SubreachBankStorageGainMinusLoss=0.0
SubreachGagedTributaryFlow=0.0
SubreachGagedMainstemFlow=0.0
SegmentDownstreamTotalNativeFlow=0.0
SegmentDownstreamTotalGagedFlow=0.0
SegmentGainOrLossInNativeFlowBetweenGagingStations=0.0
SegmentUpstreamTotalNativeFlow=0.0
SegmentUpstreamTotalGagedFlow=0.0
SegmentGagedTributaryFlow=0.0
SegmentGagedMainstemFlow=0.0
TemporarySubreachDownstreamNativeFlow=0.0
TemporarySubreachDownstreamNativeFlow2=0.0
TemporarySubreachDownstreamTotalRRF=0.0
TemporarySubreachDownstreamTotalRRF2=0.0
TemporarySubreachUpstreamNativeFlow=0.0
TemporarySubreachUpstreamNativeFlow2=0.0
TemporarySubreachUpstreamTotalRRF=0.0
TemporarySubreachUpstreamTotalRRF2=0.0

!No writes to scratch files in this subroutine for TL savings computation.
!Segment loop, from one gage downstream to next gage. Repeat each segment loop (includes one or more subreaches) until assumed and computed
!downstream streamflows converge.

NameOfCreek='FTN'
FlagForScratchOutput=.FALSE.

LoopThroughStreamSegments: do CurrentSegmentNumber=1,NumberOfFountainCreekSegments
     FountainCreekSegment=CurrentSegmentNumber+NumberOfMonumentCreekSegments
     
     !Reinitialize variables this segment.
     AccumulatedFryArkExchangeAndRRFDiversions=0.0
     TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ=0.0
     SegmentDifferenceComputedDownstreamQCurrentAndLastIteration=0.0
     SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow=0.0
     NumberOfIterationsForConvergence=0
     ConvergenceFlag=.FALSE.
     
     !Set variables for starting and ending subreach numbers in current segment.
     CheckForFirstSegmentAndAssignSegmentNumber:  if (CurrentSegmentNumber == 1) then
          BeginningSubreachInStreamSegment=1
          LastSubreachInStreamSegment=FountainCreekGagingStationNode(CurrentSegmentNumber+1)-1-NumberOfMonumentCreekSubreaches
     else if (CurrentSegmentNumber > 1 .and. CurrentSegmentNumber <NumberOfFountainCreekSegments) then
          BeginningSubreachInStreamSegment=LastSubreachInStreamSegment+1
          LastSubreachInStreamSegment=FountainCreekGagingStationNode(CurrentSegmentNumber+1)-1-NumberOfMonumentCreekSubreaches
     else if (CurrentSegmentNumber == NumberOfFountainCreekSegments) then
          BeginningSubreachInStreamSegment=LastSubreachInStreamSegment+1
          LastSubreachInStreamSegment=BeginningSubreachInStreamSegment
     end if CheckForFirstSegmentAndAssignSegmentNumber

     !Set total discharge in Fountain Creek at upstream gage of segment. Set total discharge in Fountain Creek at downstream gage. 
     !Assume that discharge at mouth (last segment) is same as usgs Pueblo gage.
     CheckForFirstSegmentAndAssignTotalQ: if (CurrentSegmentNumber < NumberOfFountainCreekSegments) then
          SegmentUpstreamTotalGagedFlow=GagingStationDischarge_Mainstem(FountainCreekSegment)
          SegmentDownstreamTotalGagedFlow=GagingStationDischarge_Mainstem(FountainCreekSegment+1)
     else
          SegmentUpstreamTotalGagedFlow=GagingStationDischarge_Mainstem(FountainCreekSegment)
          SegmentDownstreamTotalGagedFlow=SegmentUpstreamTotalGagedFlow
     end if CheckForFirstSegmentAndAssignTotalQ

     !Set value of reusable return flow at the start of the current segment. For segment 1, value is from last segment (ds) of 
     !Monument Creek reach. For other segments, value is the ending value from previous segment.
     CheckForFirstSegmentAndAssignRRF: if (CurrentSegmentNumber == 1) then
          SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)=NetRRFQInMonumentDischargedToFountain
          TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)
     else
          SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)= &
               SubreachDownstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment-1)
          TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)
     end if CheckForFirstSegmentAndAssignRRF

     !Add any new reusable return flows in current segment and subtract any diversions (take-outs) of reusable return flows for exchange, off-stream
     !storage, or other forms of removal (subroutine 'ComputeAdjustedSegmentRRF'). Results in a modified value for "TemporarySubreachUpstreamTotalRRF."
     call ComputeAdjustedSegmentRRF (TemporarySubreachUpstreamTotalRRF,CurrentSegmentNumber,FountainCreekGagingStationNode)

     !Determine if any there are any gaged tributary flows and any measured native return flows in the current segment.
     FlagForSubreachOrSegmentLevel=.FALSE.
     call ComputeTotalNativeRRFOrTributaryInflow (SegmentGagedTributaryFlow,SegmentGagedMainstemFlow,CurrentSegmentNumber, &
          FountainCreekGagingStationNode)

     !Sum transmountain diversions in current segment. Subtract value for ditches in tl savings computation.
     AccumulatedFryArkExchangeAndRRFDiversions=SegmentSumFryArkDiversionsAlongFountainCreek(CurrentSegmentNumber)+ &
          SegmentSumExchangeDiversionsAlongFountainCreek(CurrentSegmentNumber)+ &
          SegmentSumReusedDiversionsAlongFountainCreek(CurrentSegmentNumber)- &
          SegmentTransitLossFromFromFountainCreekDiversion(CurrentSegmentNumber)

     !Calculate us native flow on basis of reusable return flow, transmountain diversions, native return flow, and tributary flow. 
     !US native flow is now set and will not be modified any further for subsequent subreach iterations within current segment. 
     !DS native flow can be recalculated.
     SegmentUpstreamTotalNativeFlow=SegmentUpstreamTotalGagedFlow-(TemporarySubreachUpstreamTotalRRF+ &
          AccumulatedFryArkExchangeAndRRFDiversions)+(SegmentGagedTributaryFlow+SegmentGagedMainstemFlow)

     !Loop back to here if transit loss calculations have not converged.
     10  continue

     !Calculate conditional native flow downstream station. Assume Downstream reusable return flow is same as us value and add native diversions back in 
     !(gage data reflects the diversion). Also add in difference between assumed Downstream flows and calculated flows after each iteration (SegmentDifferenceComputedDownstreamQCurrentAndLastIteration, 
     !zero initially, is an adjustment to downstream native flow).
     SegmentDownstreamTotalNativeFlow=SegmentDownstreamTotalGagedFlow-TemporarySubreachUpstreamTotalRRF+ &
          SegmentDifferenceComputedDownstreamQCurrentAndLastIteration+ &
          SegmentSumNativeFlowDiversionsAlongFountainCreek(CurrentSegmentNumber)

     !Calculate total gain/loss this segment.
     SegmentGainOrLossInNativeFlowBetweenGagingStations=SegmentDownstreamTotalNativeFlow-SegmentUpstreamTotalNativeFlow

     !Set segment channel length. calculate native gain/loss per mile.
     SegmentNativeFlowGainOrLossPerMile=SegmentGainOrLossInNativeFlowBetweenGagingStations/ &
          FountainCreekSegmentLength(CurrentSegmentNumber)

     !Inner loop. Loop each of n subreaches within each segment.
     LoopThroughSubreachesInCurrentStreamSegment: do CurrentSubreach=BeginningSubreachInStreamSegment,LastSubreachInStreamSegment
          
          !FountainCreekSubreach is needed for variables that are dimensioned for all subreaches, not just for reaches within fountain creek reach.
          FountainCreekSubreach=CurrentSubreach+NumberOfMonumentCreekSubreaches

          !Set reusable return flow at upstream node of current subreach. For subreach 1, value is from beginning of segment loop; 
          !otherwise, from downstream node of previous subreach.
          CheckForFirstSubreachAndAssignUpstreamRRF: if (CurrentSubreach == 1) then
               SubreachUpstreamTotalRRFAlongFountainCreek(CurrentSubreach)= &
                    SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)
               TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongFountainCreek(CurrentSubreach)
          else
               SubreachUpstreamTotalRRFAlongFountainCreek(CurrentSubreach)= &
                    SubreachDownstreamTotalRRFAlongFountainCreek(CurrentSubreach-1)
               TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongFountainCreek(CurrentSubreach)
          end if CheckForFirstSubreachAndAssignUpstreamRRF

          !Determine if any new reusable return flows or diversions (take-outs) of reusable return flows for exchange, off-stream storage, 
          !or other forms Of removal are in current subreach (subroutine 'ComputePercentRRFForEachRRFEntityAtTopOfSubreach'). Transmountain diversions also are included in 
          !adjusting TemporarySubreachUpstreamTotalRRF, but in a separate subroutine. Results in an adjusted value of "TemporarySubreachUpstreamTotalRRF."
          !For the transit loss savings computation, modified versions of subroutines "ComputeAdjustedSegmentRRF" and "CheckIfCSDiversionsLessThanRRF" are used.
          
          call ComputePercentRRFForEachRRFEntityAtTopOfSubreach (TemporarySubreachUpstreamTotalRRF,FountainCreekSubreach, &
               FountainCreekGagingStationNode(CurrentSegmentNumber),NumberOfIterationsForConvergence,.TRUE.)

          !Set additional variable to use in call statements (set to zero when flow is negative).
          TemporarySubreachUpstreamTotalRRF2=TemporarySubreachUpstreamTotalRRF
          if (TemporarySubreachUpstreamTotalRRF <= 0.0) TemporarySubreachUpstreamTotalRRF2=0.0

          !Determine if there are any gaged tributary flows and/or any measured native return flows in the current subreach.
          FlagForSubreachOrSegmentLevel=.TRUE.
          call ComputeTotalNativeRRFOrTributaryInflow (SubreachGagedTributaryFlow,SubreachGagedMainstemFlow,FountainCreekSubreach, &
               FountainCreekGagingStationNode)

          !Set native flow at upstream node.
          CheckForFirstSubreachAndAssignUpstreamNativeFlow: if (CurrentSubreach == 1) then
               SubreachUpstreamNativeFlowAlongFountainCreek(CurrentSubreach)=NetNativeQInMonumentDischargedToFountain
               TemporarySubreachUpstreamNativeFlow=SubreachUpstreamNativeFlowAlongFountainCreek(CurrentSubreach)
          else
               SubreachUpstreamNativeFlowAlongFountainCreek(CurrentSubreach)= &
                    SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach-1)
               TemporarySubreachUpstreamNativeFlow=SubreachUpstreamNativeFlowAlongFountainCreek(CurrentSubreach)
          end if CheckForFirstSubreachAndAssignUpstreamNativeFlow

          !Calculate adjusted native flow at upstream node on basis of tributary flow, native return flow, and native diversion. also add in 
          !value of reusable return flow to be delivered in current subreach; these flows are converted to native flow. set additional variable 
          !to use in call statements(set to zero when flow is negative).
          TemporarySubreachUpstreamNativeFlow=TemporarySubreachUpstreamNativeFlow+(SubreachGagedTributaryFlow+ &
               SubreachGagedMainstemFlow+SubreachSumReusableFlowConvertedToNativeFlow(FountainCreekSubreach))- &
               SubreachSumNativeFlowDiversionsAlongFountainCreek(CurrentSubreach)
          TemporarySubreachUpstreamNativeFlow2=TemporarySubreachUpstreamNativeFlow
          if (TemporarySubreachUpstreamNativeFlow <= 0.0) TemporarySubreachUpstreamNativeFlow2=0.0

          !Calculate native gain/loss this subreach and native flow at downstream node. Set additional variable to use in call statements 
          !(set to zero when flow is negative).
          SubreachGainOrLossNativeFlowAlongFountainCreek(CurrentSubreach)=SegmentNativeFlowGainOrLossPerMile* &
               FountainCreekSubreachLength(CurrentSubreach)
          SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach)=TemporarySubreachUpstreamNativeFlow+ &
               SubreachGainOrLossNativeFlowAlongFountainCreek(CurrentSubreach)
          TemporarySubreachDownstreamNativeFlow=SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach)
          TemporarySubreachDownstreamNativeFlow2=TemporarySubreachDownstreamNativeFlow
          if (TemporarySubreachDownstreamNativeFlow <= 0.0) TemporarySubreachDownstreamNativeFlow2=0.0

          !Check if subreach native diversions less than total native flow and save subreach for output report. Skip/remove for transitloss savings computation.
          !Look up bank storage loss (SubreachBankStorageLossAlongFountainCreek) for current upstream reusable and native flows. If TemporarySubreachUpstreamTotalRRF < 0.5 skip subroutine.

          CheckForUpstreamRRFSoSmallThatBSLossIs0: if (TemporarySubreachUpstreamTotalRRF2 > 0.5) then
               call InterpolateBankStorageLoss (SubreachBankStorageLossAlongFountainCreek(CurrentSubreach),FountainCreekSubreach)
          else
               SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)=0.0
          end if CheckForUpstreamRRFSoSmallThatBSLossIs0

          !Calculate adjustment factor for bank storage loss (SubreachBankStorageLossAlongFountainCreek).
          CheckForNonZeroNativeFlowBeforeAdjustingBankStorageLoss: if (TemporarySubreachUpstreamNativeFlow2 > 0.0 .and. &
               TemporarySubreachDownstreamNativeFlow2 > 0.0) then
               call AdjustBankStorageLossForNonuniformNativeFlow (CurrentSubreach)
          else
               FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=1.0
          end if CheckForNonZeroNativeFlowBeforeAdjustingBankStorageLoss

          !Calculate adjusted bank storage loss.
          SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)=SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)* &
               FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow

          !Calculate total gains from bank storage. set recovery period in "call." Add losses and gains to determine net bank storage loss/gain (SubreachBankStorageGainMinusLoss).
          !Calculate bank storage loss and gains for individual reusable return flows. Method retains use of subreach values for lump sum of return flows while
          !calculating individual values. L. Miller 9-02-2009

          !Set bank storage gain for subreach to zero. Then calculate the bank storage loss for each individual return flow by applying appropriate percentage value
          !to the total subreach value for lump sum of all return flows.
          SubreachBankStorageGainAlongFountainCreek(CurrentSubreach)=0.0
          LoopThroughRRFsToEstimateNetBankStorageGainInSubreach: do CurrentRRF=1,NumberOfReusableReturnFlowsEntered
               SubreachBankStorageLossForEachRRFRelease(FountainCreekSubreach,CurrentRRF)= &
                    SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)* &
                    (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)
               !Now call subroutine to compute cumulative gains from bank storage for each individual return flow for the appropriate number of days in recovery period.
               call ComputeBankStorageRecovery (SubreachBankStorageLossForEachRRFRelease(FountainCreekSubreach,CurrentRRF), &
                    SubreachBankStorageGainForEachRRFRelease(FountainCreekSubreach,CurrentRRF),FountainCreekSubreach,CurrentRRF)
               !Lastly, sum individual bank storage gains to get total value for all return flows in the subreach.
               SubreachBankStorageGainAlongFountainCreek(CurrentSubreach)= &
                    SubreachBankStorageGainAlongFountainCreek(CurrentSubreach)+ &
                    SubreachBankStorageGainForEachRRFRelease(FountainCreekSubreach,CurrentRRF)
          end do LoopThroughRRFsToEstimateNetBankStorageGainInSubreach
     
          SubreachBankStorageGainMinusLoss=SubreachBankStorageGainAlongFountainCreek(CurrentSubreach)- &
               SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)

          !Calculate first temporary Downstream reusable return flow--used to estimate channel storage loss.
          TemporarySubreachDownstreamTotalRRF=TemporarySubreachUpstreamTotalRRF+ &
               SubreachBankStorageGainMinusLoss
          
          !Set additional variable to use in call statements (set to zero when flow is negative).
          TemporarySubreachDownstreamTotalRRF2=TemporarySubreachDownstreamTotalRRF
          if (TemporarySubreachDownstreamTotalRRF <= 0.0) TemporarySubreachDownstreamTotalRRF2=0.0

          !Calculate channel storage loss and gain.
          SubreachChannelStorageLoss=0.0
          SubreachChannelStorageGain=0.0
          call ComputeSubreachChannelStorageLoss (CurrentSubreach)

          !Calculate second temporary Downstream reusable return flow using channel storage loss/gain.
          TemporarySubreachDownstreamTotalRRF=TemporarySubreachDownstreamTotalRRF+SubreachChannelStorageGain- &
               SubreachChannelStorageLoss
          TemporarySubreachDownstreamTotalRRF2=TemporarySubreachDownstreamTotalRRF
          if (TemporarySubreachDownstreamTotalRRF <= 0.0) TemporarySubreachDownstreamTotalRRF2=0.0

          !Calculate evaporative loss. If reusable flow zero or less, no evap. loss.
          call CalculateRRFEvaporativeLoss (SubreachEvaporationLossFountainCreek(CurrentSubreach), &
               FountainCreekSubreachLength(CurrentSubreach))

          !Calculate total reusable gain/loss this subreach and reusable return flow at downstream node. Set value of downstream native flow.
          CheckForConvergenceAndCalcDownstreamRRFForSubreach: if (.not. ConvergenceFlag) then
               SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)= &
                    (SubreachBankStorageGainMinusLoss+SubreachChannelStorageGain)- &
                    (SubreachChannelStorageLoss+SubreachEvaporationLossFountainCreek(CurrentSubreach))
               SubreachDownstreamTotalRRFAlongFountainCreek(CurrentSubreach)=TemporarySubreachUpstreamTotalRRF+ &
                    SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)
               SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach)=TemporarySubreachDownstreamNativeFlow

          !Calculate downstream flow quantities for each individual reusable return-flow entity for current subreach on basis of percentages
          !computed at beginning of subreach. Only when converged.

          !Calculate channel storage loss/gain and evaporative loss for each individual return flow using appropriate percentage value calculated earlier.
          !Sum all transit loss/gain values for each individual return flow to get total transit loss/gain values for current subreach.
          else if (ConvergenceFlag) then
               SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)=0.0
              
               LoopThroughAllRRFsToCalcTotalTLForCurrentSubreach: do CurrentRRF=1,NumberOfReusableReturnFlowsEntered
                    TemporaryRRF=SubreachBankStorageGainForEachRRFRelease(FountainCreekSubreach,CurrentRRF)- &
                         SubreachBankStorageLossForEachRRFRelease(FountainCreekSubreach,CurrentRRF)+SubreachChannelStorageGain* &
                         (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)- &
                         SubreachChannelStorageLoss*(FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)+ &
                         SubreachEvaporationLossFountainCreek(CurrentSubreach)* &
                         (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)
                    !Calculate downstream values for individual return flow entities and subreach value of total transit loss/gain.
                    DownstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,CurrentRRF)= &
                         UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,CurrentRRF)+TemporaryRRF
                    UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach+1,CurrentRRF)= &
                         DownstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,CurrentRRF)
                    SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)= &
                         SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)+TemporaryRRF

                    !If at end of last subreach, set delivery flows. 
                    CheckForLastFountainCreekSubreach: &
                         if (FountainCreekSubreach >= NumberOfMonumentCreekSubreaches+NumberOfFountainCreekSubreaches) then
                              !Set delivery discharge for specific entity.
                              if (ReusableReturnFlowDeliveryNode(CurrentRRF) >= &
                                   NumberOfMonumentCreekSubreaches+NumberOfFountainCreekSubreaches) then
                                        RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)= &
                                             DownstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,CurrentRRF)
                                        !Set value for delivery discharge for csu transmountain return flow without the transit loss savings computation.
                                        if (CurrentRRF == 5) CSURRFDeliveryWithoutTransitLossSavings= &
                                             RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)
                                   end if
                    end if CheckForLastFountainCreekSubreach

               end do LoopThroughAllRRFsToCalcTotalTLForCurrentSubreach
               
               !Calculate net values at downstream node of current subreach.
               SubreachDownstreamTotalRRFAlongFountainCreek(CurrentSubreach)=TemporarySubreachUpstreamTotalRRF+ &
                    SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)
               SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach)=TemporarySubreachDownstreamNativeFlow
          end if CheckForConvergenceAndCalcDownstreamRRFForSubreach 

          !When converged, calculate percentage reusable return flow loss/gain and percentage native loss/gain for each subreach (display on output).
          !Skip/remove for TL savings subroutine computations.
          !Skip/removed "CalculateReleasesNeededForSpecifiedExchangeDiversion" for tl savings.

     !End of subreach loop.
     end do LoopThroughSubreachesInCurrentStreamSegment

     !Calculate percent difference between assumed Downstream flow and calculated Downstream flow after subreach computations (segment loop).
     TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ=SubreachDownstreamTotalRRFAlongFountainCreek(CurrentSubreach-1)+ &
          SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach-1)
     SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow=SegmentDownstreamTotalGagedFlow- &
          TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ
     CheckForDownstreamSubreachGTZero: if (SegmentDownstreamTotalGagedFlow > 0.0) then
          PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest= &
               SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow/SegmentDownstreamTotalGagedFlow*100.
     else
          PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest=0.0
     end if CheckForDownstreamSubreachGTZero

     !See if difference is within 0.5% criteria. If not converged, loop back into segment computation without resetting/incrementing upstream variables.
     !Note that a final set of subreach iterations is made after segment iterations converge. Last iteration is needed so that bank-storage and channel storage
     !values can be written to appropriate files and that downstream/upstream percentages of each reusable return-flow entity can be calculated.
     CheckForFailureToConverge: if (.not. ConvergenceFlag) then
          CheckForMoreThan20Iterations: if (NumberOfIterationsForConvergence >= 20) then
               ConvergenceFlag=.TRUE.
               !No writes to scratch files for tl savings computation.
               !write (SCRATCH2FileUnit,1040) CurrentSegmentNumber
          else if (abs(PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest) < 0.1) then
               ConvergenceFlag=.TRUE.
               NumberOfIterationsForConvergence=NumberOfIterationsForConvergence+1
          else if (abs(PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest) >= 0.1) then
               SegmentDifferenceComputedDownstreamQCurrentAndLastIteration= &
                    SegmentDifferenceComputedDownstreamQCurrentAndLastIteration+ &
                    SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow
               NumberOfIterationsForConvergence=NumberOfIterationsForConvergence+1
          end if CheckForMoreThan20Iterations
          goto 10
     end if CheckForFailureToConverge

!End of segment loop.
end do LoopThroughStreamSegments

!Write out updates to accounts file. Not used here.
!Copy scratch file of bank storage values to disk file. Not used here.

return

!No writes to scratch files for TL savings computation.
! 1000 format ('subreach ',i2.2)
! 1010 format (',',i2)
! 1020 format (' ',i2)
! 1030 format ('ru return flow ',i3.3,': ', a56)
! 1040 format ('ICCHK convergence criteria not met after 20 iterations in Fountain Creek segment',i2,'.')

end

!*****************************************************************************
subroutine EstimateFountainCreekRRFTransitLosses (NetRRFQInMonumentDischargedToFountain,NetNativeQInMonumentDischargedToFountain, &
     InputErrorFlag)
!*****************************************************************************
!Subroutine computes (estimates) transit-losses for reusable return flows along Fountain Creek. Numerous other subroutines are called.

!Definition of local variables:
!-----------------------------------------------------------------------------
!LastSubreachInStreamSegment = ending subreach within a stream segment (between gaging stations).
!BeginningSubreachInStreamSegment = beginning subreach within a stream segment (between gaging stations).
!AccumulatedFryArkExchangeAndRRFDiversions = sum of all transmountain diversions in current subreach.
!NumberOfIterationsForConvergence = the number of iterations made for the stream segment and subreach loops before the convergence criterion is met.
!FountainCreekSegment = local variables to indicate number for segment or subreach loops.
!FountainCreekSubreach = local variables to indicate number for segment or subreach loops.
!NetRRFQInMonumentDischargedToFountain = the net (total) amount of reusable return flow at the end of Monument Creek reach (carried from subroutine EstimateFountainCreekRRFTransitLosses).
!NetNativeQInMonumentDischargedToFountain = the net (total) amount of native flow at the end of Monument Creek reach (carried from subroutine EstimateFountainCreekRRFTransitLosses).
!*****************************************************************************
use CommonVariables, &
     only : ACCTSFileUnit,AmountOfCSExchangeWaterUsed,AmountOfCSFryArkWaterUsed,AmountOfCSWaterUsed, &
     ConvergenceFlag,CSRRFDiversion2AlongFountainCreek,CSURRFDeliveryWithTransitLossSavings, &
     DownstreamReusableReturnFlowByRRFEntity,ExchangeDiversion1AlongFountainCreek, &
     FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow,FlagForExchangeDiversion,FlagForFryArkDiversion, &
     FlagForRRFDiversion,FlagForScratchOutput,FlagForSubreachOrSegmentLevel,FountainCreekGagingStationNode, &
     GagingStationDischarge_Mainstem,FountainCreekSegmentLength,FountainCreekSubreachLength, &
     FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions,FryArkFlowDiversionAlongFountainCreek,NameOfCreek, &
     NumberOfFountainCreekDiversionDitches,NumberOfFountainCreekSegments,NumberOfFountainCreekSubreaches, &
     NumberOfMonumentCreekSegments,NumberOfMonumentCreekSubreaches,NumberOfReusableReturnFlowsEntered, &
     NumberSubreachesNativeDiversionsGTNativeFlow,PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest, &
     ReleasedReusableReturnFlowName,ReusableReturnFlowDeliveryNode,RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions, &
     SCRATCH1FileUnit,SCRATCH2FileUnit,SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow, &
     SegmentDifferenceComputedDownstreamQCurrentAndLastIteration,SegmentDownstreamTotalGagedFlow,SegmentDownstreamTotalNativeFlow,&
     SegmentGagedMainstemFlow,SegmentGagedTributaryFlow,SegmentGainOrLossInNativeFlowBetweenGagingStations, &
     SegmentNativeFlowGainOrLossPerMile,SegmentSumExchangeDiversionsAlongFountainCreek, &
     SegmentSumFryArkDiversionsAlongFountainCreek,SegmentSumNativeFlowDiversionsAlongFountainCreek, &
     SegmentSumReusedDiversionsAlongFountainCreek,SegmentUpstreamTotalGagedFlow,SegmentUpstreamTotalNativeFlow, &
     SubreachBankStorageGainAlongFountainCreek,SubreachBankStorageGainForEachRRFRelease,SubreachBankStorageGainMinusLoss, &
     SubreachBankStorageLossAlongFountainCreek,SubreachBankStorageLossForEachRRFRelease,SubreachChannelStorageGain, &
     SubreachChannelStorageLoss,SubreachDownstreamNativeFlowAlongFountainCreek,SubreachDownstreamTotalRRFAlongFountainCreek, &
     SubreachesNativeFlowLessThan0,SubreachEvaporationLossFountainCreek,SubreachGagedMainstemFlow,SubreachGagedTributaryFlow, &
     SubreachGainOrLossAllReusableFlowAlongFountainCreek,SubreachGainOrLossNativeFlowAlongFountainCreek, &
     SubreachGainOrLossNativeFlowAlongFountainCreek_pct,SubreachGainOrLossReusableFlowAlongFountainCreek_pct, &
     SubreachSumExchangeDiversionsAlongFountainCreek,SubreachSumFryArkDiversionsAlongFountainCreek, &
     SubreachSumNativeFlowDiversionsAlongFountainCreek,SubreachSumReusableFlowConvertedToNativeFlow, &
     SubreachSumReusedDiversionsAlongFountainCreek,SubreachUpstreamNativeFlowAlongFountainCreek, &
     SubreachUpstreamTotalRRFAlongFountainCreek,TemporarySubreachDownstreamNativeFlow,TemporarySubreachDownstreamNativeFlow2, &
     TemporarySubreachDownstreamTotalRRF,TemporarySubreachDownstreamTotalRRF2,TemporarySubreachUpstreamNativeFlow, &
     TemporarySubreachUpstreamNativeFlow2,TemporarySubreachUpstreamTotalRRF,TemporarySubreachUpstreamTotalRRF2, &
     TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ,UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow,&
     UpstreamReleaseInFountainCreekNeededToDeliverFryArkReturnFlow,UpstreamReleaseInFountainCreekNeededToDeliverReusedReturnFlow, &
     UpstreamReusableReturnFlowByRRFEntity
implicit none

!#Create an interface so that you can pass an optional argument to subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach.
interface
    subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach(TotalRRFForCurrentSubreach,CurrentSubreach, &
          CurrentSegment,NumberOfIterationsForConvergence,PerformTransitLossSavingsComputation)
          integer :: CurrentSubreach,CurrentSegment,NumberOfIterationsForConvergence,CurrentRRFInSubreach,CurrentRRFDiversion, &
	       CurrentRRFEntity,CurrentRRFAtNode,CurrentRRF
	  logical, intent(in), optional ::    PerformTransitLossSavingsComputation
	  real :: TotalRRFForCurrentSubreach
     end subroutine ComputePercentRRFForEachRRFEntityAtTopOfSubreach
end interface

real, intent(in) :: NetRRFQInMonumentDischargedToFountain,NetNativeQInMonumentDischargedToFountain
integer, intent(out) :: InputErrorFlag
integer :: BeginningSubreachInStreamSegment,LastSubreachInStreamSegment,FountainCreekSegment,FountainCreekSubreach, &
     NumberOfIterationsForConvergence,CurrentSegmentNumber,CurrentSubreach,CurrentRRF
real :: AccumulatedFryArkExchangeAndRRFDiversions,TemporaryRRF

FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=1.0
SegmentNativeFlowGainOrLossPerMile=0.0
PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest=0.0
SubreachBankStorageGainMinusLoss=0.0
SubreachGagedTributaryFlow=0.0
SubreachGagedMainstemFlow=0.0
SegmentDownstreamTotalNativeFlow=0.0
SegmentDownstreamTotalGagedFlow=0.0
SegmentGainOrLossInNativeFlowBetweenGagingStations=0.0
SegmentUpstreamTotalNativeFlow=0.0
SegmentUpstreamTotalGagedFlow=0.0
SegmentGagedTributaryFlow=0.0
SegmentGagedMainstemFlow=0.0
TemporarySubreachDownstreamNativeFlow=0.0
TemporarySubreachDownstreamNativeFlow2=0.0
TemporarySubreachDownstreamTotalRRF=0.0
TemporarySubreachDownstreamTotalRRF2=0.0
TemporarySubreachUpstreamNativeFlow=0.0
TemporarySubreachUpstreamNativeFlow2=0.0
TemporarySubreachUpstreamTotalRRF=0.0
TemporarySubreachUpstreamTotalRRF2=0.0

!Segment loop, from one gage downstream to next gage. Repeat each segment loop (includes one or more subreaches) until assumed and computed
!downstream streamflows converge.
NameOfCreek='FTN'
FlagForScratchOutput=.TRUE.

LoopThroughStreamSegments:  do CurrentSegmentNumber=1,NumberOfFountainCreekSegments
     FountainCreekSegment=CurrentSegmentNumber+NumberOfMonumentCreekSegments

     !Reinitialize variables this segment.
     AccumulatedFryArkExchangeAndRRFDiversions=0.0
     TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ=0.0
     SegmentDifferenceComputedDownstreamQCurrentAndLastIteration=0.0
     SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow=0.0
     NumberOfIterationsForConvergence=0
     ConvergenceFlag=.FALSE.

     !Set variables for starting and ending subreach numbers in current segment.
     CheckSegmentNumberAndAssignStartAndEndSubreachNumbers: if (CurrentSegmentNumber == 1) then
          BeginningSubreachInStreamSegment=1
          LastSubreachInStreamSegment=FountainCreekGagingStationNode(CurrentSegmentNumber+1)-1-NumberOfMonumentCreekSubreaches
     else if (CurrentSegmentNumber > 1 .and. CurrentSegmentNumber < NumberOfFountainCreekSegments) then
          BeginningSubreachInStreamSegment=LastSubreachInStreamSegment+1
          LastSubreachInStreamSegment=FountainCreekGagingStationNode(CurrentSegmentNumber+1)-1-NumberOfMonumentCreekSubreaches
     else if (CurrentSegmentNumber == NumberOfFountainCreekSegments) then
          BeginningSubreachInStreamSegment=LastSubreachInStreamSegment+1
          LastSubreachInStreamSegment=BeginningSubreachInStreamSegment
     end if CheckSegmentNumberAndAssignStartAndEndSubreachNumbers

     !Set total discharge in Fountain Creek at upstream gage of segment. Set total discharge in Fountain Creek at downstream gage. Assume that
     !discharge at mouth (last segment) is same as USGS Pueblo gage.
     CheckForLastSegmentAndAssignTotalSegmentFlow: if (CurrentSegmentNumber < NumberOfFountainCreekSegments) then
          SegmentUpstreamTotalGagedFlow=GagingStationDischarge_Mainstem(FountainCreekSegment)
          SegmentDownstreamTotalGagedFlow=GagingStationDischarge_Mainstem(FountainCreekSegment+1)
     else
          SegmentUpstreamTotalGagedFlow=GagingStationDischarge_Mainstem(FountainCreekSegment)
          SegmentDownstreamTotalGagedFlow=SegmentUpstreamTotalGagedFlow
     end if CheckForLastSegmentAndAssignTotalSegmentFlow

     !Set value of reusable return flow at the start of the current segment. For segment 1, value is from last segment (DS) of Monument Creek reach.
     !For other segments, value is the ending value from previous segment.
     CheckForFirstSegmentAndAssignRRF: if (CurrentSegmentNumber == 1) then
          SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)=NetRRFQInMonumentDischargedToFountain
          TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)
     else
          SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)= &
               SubreachDownstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment-1)
          TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)
     end if CheckForFirstSegmentAndAssignRRF

     !Add any new reusable return flows in current segment and subtract any diversions (take-outs) of reusable return flows for exchange, off-stream
     !storage, or other forms of removal (subroutine 'ComputeAdjustedSegmentRRF'). Results in a modified value for "TemporarySubreachUpstreamTotalRRF."
     call ComputeAdjustedSegmentRRF (TemporarySubreachUpstreamTotalRRF,CurrentSegmentNumber,FountainCreekGagingStationNode)

     !Determine if any there are any gaged tributary flows and any measured native return flows in the current segment.
     FlagForSubreachOrSegmentLevel=.FALSE.
     call ComputeTotalNativeRRFOrTributaryInflow (SegmentGagedTributaryFlow,SegmentGagedMainstemFlow,CurrentSegmentNumber, &
          FountainCreekGagingStationNode)

     !Sum transmountain diversions in current segment.
     AccumulatedFryArkExchangeAndRRFDiversions=SegmentSumFryArkDiversionsAlongFountainCreek(CurrentSegmentNumber)+ &
          SegmentSumExchangeDiversionsAlongFountainCreek(CurrentSegmentNumber)+ &
          SegmentSumReusedDiversionsAlongFountainCreek(CurrentSegmentNumber)
     
     !Calculate US native flow on basis of reusable return flow, transmountain diversions, native return flow, and tributary flow. US native flow is now
     !set and will not be modified any further for subsequent subreach iterations within current segment. Downstream native flow can be recalculated.
     SegmentUpstreamTotalNativeFlow=SegmentUpstreamTotalGagedFlow-(TemporarySubreachUpstreamTotalRRF+ &
          AccumulatedFryArkExchangeAndRRFDiversions)+(SegmentGagedTributaryFlow+SegmentGagedMainstemFlow)

     !Loop back to here if transit loss calculations have not converged.
     10 continue

     !Calculate conditional native flow downstream station. Assume Downstream reusable return flow is same as US value and add native diversions back in (gage data
     !reflects the diversion). Also add in difference between assumed Downstream flows and calculated flows after each iteration (SegmentDifferenceComputedDownstreamQCurrentAndLastIteration, zero initially, is an
     !adjustment to downstream native flow).
     SegmentDownstreamTotalNativeFlow=SegmentDownstreamTotalGagedFlow-TemporarySubreachUpstreamTotalRRF+ &
          SegmentDifferenceComputedDownstreamQCurrentAndLastIteration+ &
          SegmentSumNativeFlowDiversionsAlongFountainCreek(CurrentSegmentNumber)

     !Calculate total gain/loss this segment.
     SegmentGainOrLossInNativeFlowBetweenGagingStations=SegmentDownstreamTotalNativeFlow-SegmentUpstreamTotalNativeFlow

     !Set segment channel length. calculate native gain/loss per mile.
     SegmentNativeFlowGainOrLossPerMile=SegmentGainOrLossInNativeFlowBetweenGagingStations/ &
          FountainCreekSegmentLength(CurrentSegmentNumber)

     !Inner loop. Loop each of n subreaches within each segment.
     LoopThroughSubreachesInCurrentStreamSegment: do CurrentSubreach=BeginningSubreachInStreamSegment,LastSubreachInStreamSegment

          !FountainCreekSubreach is needed for variables that are dimensioned for all subreaches, not just for reaches within Fountain Creek reach.
          FountainCreekSubreach=CurrentSubreach+NumberOfMonumentCreekSubreaches

          !Write subreach number to scratch1.
          if (ConvergenceFlag) then
               write (SCRATCH1FileUnit,1000) FountainCreekSubreach
          end if

          !Set reusable return flow at upstream node of current subreach. For subreach 1, value is from beginning of segment loop; otherwise, from
          !downstream node of previous subreach.
          CheckForFirstSubreachAndAssignUpstreamRRF: if (CurrentSubreach == 1) then
               SubreachUpstreamTotalRRFAlongFountainCreek(CurrentSubreach)= &
                    SubreachUpstreamTotalRRFAlongFountainCreek(BeginningSubreachInStreamSegment)
               TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongFountainCreek(CurrentSubreach)
          else
               SubreachUpstreamTotalRRFAlongFountainCreek(CurrentSubreach)= &
                    SubreachDownstreamTotalRRFAlongFountainCreek(CurrentSubreach-1)
               TemporarySubreachUpstreamTotalRRF=SubreachUpstreamTotalRRFAlongFountainCreek(CurrentSubreach)
          end if CheckForFirstSubreachAndAssignUpstreamRRF

          !Determine if any new reusable return flows or diversions (take-outs) of reusable return flows for exchange, off-stream storage, or other forms
          !of removal are in current subreach (subroutine 'ComputePercentRRFForEachRRFEntityAtTopOfSubreach'). Transmountain diversions also are included in adjusting TemporarySubreachUpstreamTotalRRF, but in a separate
          !subroutine. Results in an adjusted value of "TemporarySubreachUpstreamTotalRRF."
          
          call ComputePercentRRFForEachRRFEntityAtTopOfSubreach (TemporarySubreachUpstreamTotalRRF,FountainCreekSubreach, &
               FountainCreekGagingStationNode(CurrentSegmentNumber),NumberOfIterationsForConvergence)

          !Set additional variable to use in call statements (set to zero when flow is negative).
          TemporarySubreachUpstreamTotalRRF2=TemporarySubreachUpstreamTotalRRF
          if (TemporarySubreachUpstreamTotalRRF <= 0.0) TemporarySubreachUpstreamTotalRRF2=0.0

          !Determine if there are any gaged tributary flows and/or any measured native return flows in the current subreach.
          FlagForSubreachOrSegmentLevel=.TRUE.
          call ComputeTotalNativeRRFOrTributaryInflow (SubreachGagedTributaryFlow,SubreachGagedMainstemFlow,FountainCreekSubreach, &
               FountainCreekGagingStationNode)

          !Set native flow at upstream node.
          CheckForFirstSubreachAndAssignUpstreamNativeFlow: if (CurrentSubreach == 1) then
               SubreachUpstreamNativeFlowAlongFountainCreek(CurrentSubreach)=NetNativeQInMonumentDischargedToFountain
               TemporarySubreachUpstreamNativeFlow=SubreachUpstreamNativeFlowAlongFountainCreek(CurrentSubreach)
          else
               SubreachUpstreamNativeFlowAlongFountainCreek(CurrentSubreach)= &
                    SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach-1)
               TemporarySubreachUpstreamNativeFlow=SubreachUpstreamNativeFlowAlongFountainCreek(CurrentSubreach)
          end if CheckForFirstSubreachAndAssignUpstreamNativeFlow 

          !Calculate adjusted native flow at upstream node on basis of tributary flow, native return flow, and native diversion. Also add in value of reusable
          !return flow to be delivered in current subreach; these flows are converted to native flow. set additional variable to use in call statements
          !(set to zero when flow is negative).
          TemporarySubreachUpstreamNativeFlow=TemporarySubreachUpstreamNativeFlow+(SubreachGagedTributaryFlow+ &
               SubreachGagedMainstemFlow+SubreachSumReusableFlowConvertedToNativeFlow(FountainCreekSubreach))- &
               SubreachSumNativeFlowDiversionsAlongFountainCreek(CurrentSubreach)
          TemporarySubreachUpstreamNativeFlow2=TemporarySubreachUpstreamNativeFlow
          if (TemporarySubreachUpstreamNativeFlow <= 0.0) TemporarySubreachUpstreamNativeFlow2=0.0

          !Calculate native gain/loss this subreach and native flow at downstream node. Set additional variable to use in call statements (set to zero when
          !flow is negative).
          SubreachGainOrLossNativeFlowAlongFountainCreek(CurrentSubreach)=SegmentNativeFlowGainOrLossPerMile* &
               FountainCreekSubreachLength(CurrentSubreach)
          SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach)=TemporarySubreachUpstreamNativeFlow+ &
               SubreachGainOrLossNativeFlowAlongFountainCreek(CurrentSubreach)
          TemporarySubreachDownstreamNativeFlow=SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach)
          TemporarySubreachDownstreamNativeFlow2=TemporarySubreachDownstreamNativeFlow
          if (TemporarySubreachDownstreamNativeFlow <= 0.0) TemporarySubreachDownstreamNativeFlow2=0.0

          !Check if subreach native diversions less than total native flow and save subreach for output report.
          CheckForNegativeDownstreamSubreachNativeFlow: if (SubreachSumNativeFlowDiversionsAlongFountainCreek(CurrentSubreach) > &
               0.0 .and. TemporarySubreachDownstreamNativeFlow <= 0.0 .and. ConvergenceFlag) then
               NumberSubreachesNativeDiversionsGTNativeFlow=NumberSubreachesNativeDiversionsGTNativeFlow+1
               CheckForMoreThan1SubreachWithNegativeDownstreamNativeFlow:if (NumberSubreachesNativeDiversionsGTNativeFlow > 1) then
                  write (SubreachesNativeFlowLessThan0(NumberSubreachesNativeDiversionsGTNativeFlow),1010) FountainCreekSubreach
               else
                  write (SubreachesNativeFlowLessThan0(NumberSubreachesNativeDiversionsGTNativeFlow),1020) FountainCreekSubreach
               end if CheckForMoreThan1SubreachWithNegativeDownstreamNativeFlow
          end if CheckForNegativeDownstreamSubreachNativeFlow

          !Look up bank storage loss (SubreachBankStorageLossAlongFountainCreek) for current upstream reusable and native flows. If TemporarySubreachUpstreamTotalRRF < 0.5 skip subroutine.
          CheckForRRFLargeEnoughToHaveBankStorageLoss: if (TemporarySubreachUpstreamTotalRRF2 > 0.5) then
               call InterpolateBankStorageLoss (SubreachBankStorageLossAlongFountainCreek(CurrentSubreach),FountainCreekSubreach)
          else
               SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)=0.0
          end if CheckForRRFLargeEnoughToHaveBankStorageLoss

          !Calculate adjustment factor for bank storage loss (SubreachBankStorageLossAlongFountainCreek).
          CheckForNonZeroNativeFlowAndBSLossAdjustmentFactorNE1: if (TemporarySubreachUpstreamNativeFlow2 > 0.0 .and. &
               TemporarySubreachDownstreamNativeFlow2 >  0.0) then
               call AdjustBankStorageLossForNonuniformNativeFlow (CurrentSubreach)
          else
               FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow=1.0
          end if CheckForNonZeroNativeFlowAndBSLossAdjustmentFactorNE1

          !Calculate adjusted bank storage loss.
          SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)=SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)* &
               FactorToAdjustSegmentBankStorageLossBasedOnNonUniformNativeFlow

          !Calculate total gains from bank storage. set recovery period in "call." Add losses and gains to determine net bank storage loss/gain (SubreachBankStorageGainMinusLoss).

          !Calculate bank storage loss and gains for individual reusable return flows. Method retains use of subreach values for lump sum of return flows while
          !calculating individual values. L. Miller 9-02-2009

          !Set bank storage gain for subreach to zero. Then calculate the bank storage loss for each individual return flow by applying appropriate percentage value
          !to the total subreach value for lump sum of all return flows.
          SubreachBankStorageGainAlongFountainCreek(CurrentSubreach)=0.0
          
          LoopThroughRRFsToEstimateNetBankStorageGainInSubreach: &
          do CurrentRRF=1,NumberOfReusableReturnFlowsEntered
               SubreachBankStorageLossForEachRRFRelease(FountainCreekSubreach,CurrentRRF)= &
                    SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)* &
                    (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)
               if (ConvergenceFlag) then
                    write (SCRATCH1FileUnit,1030) CurrentRRF,ReleasedReusableReturnFlowName(CurrentRRF)
               end if 
               !Now call subroutine to compute cumulative gains from bank storage for each individual return flow for the appropriate number of days in recovery period.
               call ComputeBankStorageRecovery (SubreachBankStorageLossForEachRRFRelease(FountainCreekSubreach,CurrentRRF), &
                    SubreachBankStorageGainForEachRRFRelease(FountainCreekSubreach,CurrentRRF),FountainCreekSubreach,CurrentRRF)
               !Lastly, sum individual bank storage gains to get total value for all return flows in the subreach.
               SubreachBankStorageGainAlongFountainCreek(CurrentSubreach)= &
                    SubreachBankStorageGainAlongFountainCreek(CurrentSubreach)+ &
                    SubreachBankStorageGainForEachRRFRelease(FountainCreekSubreach,CurrentRRF)
          end do LoopThroughRRFsToEstimateNetBankStorageGainInSubreach

          SubreachBankStorageGainMinusLoss=SubreachBankStorageGainAlongFountainCreek(CurrentSubreach)- &
               SubreachBankStorageLossAlongFountainCreek(CurrentSubreach)

          !Calculate first temporary Downstream reusable return flow--used to estimate channel storage loss.
          TemporarySubreachDownstreamTotalRRF=TemporarySubreachUpstreamTotalRRF+ &
               SubreachBankStorageGainMinusLoss

          !Set additional variable to use in call statements (set to zero when flow is negative).
          TemporarySubreachDownstreamTotalRRF2=TemporarySubreachDownstreamTotalRRF
          if (TemporarySubreachDownstreamTotalRRF <= 0.0) TemporarySubreachDownstreamTotalRRF2=0.0

          !Calculate channel storage loss and gain.
          SubreachChannelStorageLoss=0.0
          SubreachChannelStorageGain=0.0
          call ComputeSubreachChannelStorageLoss (CurrentSubreach)

          !Calculate second temporary Downstream reusable return flow using channel storage loss/gain.
          TemporarySubreachDownstreamTotalRRF=TemporarySubreachDownstreamTotalRRF+SubreachChannelStorageGain- &
               SubreachChannelStorageLoss
          TemporarySubreachDownstreamTotalRRF2=TemporarySubreachDownstreamTotalRRF
          if (TemporarySubreachDownstreamTotalRRF <= 0.0) TemporarySubreachDownstreamTotalRRF2=0.0

          !Calculate evaporative loss. If reusable flow zero or less, no evap. loss.
          call CalculateRRFEvaporativeLoss (SubreachEvaporationLossFountainCreek(CurrentSubreach), &
               FountainCreekSubreachLength(CurrentSubreach))

          !Calculate total reusable gain/loss this subreach and reusable return flow at downstream node. Set value of downstream native flow.
          CheckForConvergenceAndCalculateDownstreamRRFForSubreach: if (.not. ConvergenceFlag) then
               SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)= &
                    (SubreachBankStorageGainMinusLoss+SubreachChannelStorageGain)- &
                    (SubreachChannelStorageLoss+SubreachEvaporationLossFountainCreek(CurrentSubreach))
               SubreachDownstreamTotalRRFAlongFountainCreek(CurrentSubreach)=TemporarySubreachUpstreamTotalRRF+ &
                    SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)
               SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach)=TemporarySubreachDownstreamNativeFlow

               !Calculate downstream flow quantities for each individual reusable return-flow entity for current subreach on basis of percentages
               !computed at beginning of subreach. Only when converged.

               !Calculate channel storage loss/gain and evaporative loss for each individual return flow using appropriate percentage value calculated earlier.
               !Sum all transit loss/gain values for each individual return flow to get total transit loss/gain values for current subreach 

          else if (ConvergenceFlag) then
               SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)=0.0
               LoopThroughAllRRFsToCalculateTotalTransitLossForCurrentSubreach: do CurrentRRF=1,NumberOfReusableReturnFlowsEntered
                    TemporaryRRF=SubreachBankStorageGainForEachRRFRelease(FountainCreekSubreach,CurrentRRF)- &
                         SubreachBankStorageLossForEachRRFRelease(FountainCreekSubreach,CurrentRRF)+SubreachChannelStorageGain* &
                         (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)- &
                         SubreachChannelStorageLoss*(FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)+ &
                         SubreachEvaporationLossFountainCreek(CurrentSubreach)* &
                         (FractionRRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)/100.)
                    !Calculate downstream values for individual return flow entities and subreach value of total transit loss/gain.
                    DownstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,CurrentRRF)= &
                         UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,CurrentRRF)+TemporaryRRF
                    UpstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach+1,CurrentRRF)= &
                         DownstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,CurrentRRF)
                    SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)= &
                         SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)+TemporaryRRF

                    !If at end of last subreach, set delivery flows.
                    CheckForLastSubreachAndSetDeliveryFlow: &
                         if (FountainCreekSubreach >= NumberOfMonumentCreekSubreaches+NumberOfFountainCreekSubreaches) then
                              !Set delivery discharge for specific entity.
                              CheckForLastDelveryNodeAndSetDeliveryFlow: &
                                   if (ReusableReturnFlowDeliveryNode(CurrentRRF) >= &
                                        NumberOfMonumentCreekSubreaches+NumberOfFountainCreekSubreaches) then
                                             RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)= &
                                                  DownstreamReusableReturnFlowByRRFEntity(FountainCreekSubreach,CurrentRRF)
                                             !Set value for delivery discharge for CSU transmountain return flow with the transit loss savings computation.
                                             if (CurrentRRF == 5) CSURRFDeliveryWithTransitLossSavings = &
                                                  RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(CurrentRRF)
                                         end if CheckForLastDelveryNodeAndSetDeliveryFlow
                    end if CheckForLastSubreachAndSetDeliveryFlow
               end do LoopThroughAllRRFsToCalculateTotalTransitLossForCurrentSubreach 

               !Calculate net values at downstream node of current subreach.
               SubreachDownstreamTotalRRFAlongFountainCreek(CurrentSubreach)=TemporarySubreachUpstreamTotalRRF+ &
                    SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)
               SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach)=TemporarySubreachDownstreamNativeFlow
          end if CheckForConvergenceAndCalculateDownstreamRRFForSubreach

          !When converged, calculate percentage reusable return flow loss/gain and percentage native loss/gain for each subreach (display on output).
          CheckForConvergenceAndCalculatePercentNetRRFGainForSubreach: if (ConvergenceFlag) then
               CheckForNonZeroNetRRFGain: if (abs(TemporarySubreachUpstreamTotalRRF) > 0.0) then
                    SubreachGainOrLossReusableFlowAlongFountainCreek_pct(CurrentSubreach)= &
                         (SubreachGainOrLossAllReusableFlowAlongFountainCreek(CurrentSubreach)/ &
                         abs(TemporarySubreachUpstreamTotalRRF))*100.
               else
                    SubreachGainOrLossReusableFlowAlongFountainCreek_pct(CurrentSubreach)=0.0
               end if CheckForNonZeroNetRRFGain
               CheckForNonZeroNetNativeFlowGain: if (abs(TemporarySubreachUpstreamNativeFlow) > 0.0) then
                    SubreachGainOrLossNativeFlowAlongFountainCreek_pct(CurrentSubreach)= &
                          (SubreachGainOrLossNativeFlowAlongFountainCreek(CurrentSubreach)/ &
                          abs(TemporarySubreachUpstreamNativeFlow))*100.
               else
                    SubreachGainOrLossNativeFlowAlongFountainCreek_pct(CurrentSubreach)=0.0
               end if CheckForNonZeroNetNativeFlowGain

               !Call CalculateReleasesNeededForSpecifiedExchangeDiversion routine to estimate release requirements for each type of transmountain diversion. Skip back calculation until convergence.
               if (FlagForFryArkDiversion .and. SubreachSumFryArkDiversionsAlongFountainCreek(CurrentSubreach) > 0.0) &
                    call CalculateReleasesNeededForSpecifiedExchangeDiversion ( &
                         FryArkFlowDiversionAlongFountainCreek, &
                         UpstreamReleaseInFountainCreekNeededToDeliverFryArkReturnFlow, &
                         SubreachDownstreamTotalRRFAlongFountainCreek,SubreachGainOrLossAllReusableFlowAlongFountainCreek, &
                         CurrentSubreach)
               if (FlagForExchangeDiversion .and. SubreachSumExchangeDiversionsAlongFountainCreek(CurrentSubreach) > 0.0) &
                    call CalculateReleasesNeededForSpecifiedExchangeDiversion ( &
                         ExchangeDiversion1AlongFountainCreek, &
                         UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow, &
                         SubreachDownstreamTotalRRFAlongFountainCreek,SubreachGainOrLossAllReusableFlowAlongFountainCreek, &
                         CurrentSubreach)
               if (FlagForRRFDiversion .and. SubreachSumReusedDiversionsAlongFountainCreek(CurrentSubreach) > 0.0) &
                    call CalculateReleasesNeededForSpecifiedExchangeDiversion ( &
                         CSRRFDiversion2AlongFountainCreek, &
                         UpstreamReleaseInFountainCreekNeededToDeliverReusedReturnFlow, &
                         SubreachDownstreamTotalRRFAlongFountainCreek,SubreachGainOrLossAllReusableFlowAlongFountainCreek, &
                         CurrentSubreach)
          end if CheckForConvergenceAndCalculatePercentNetRRFGainForSubreach

     !End of subreach loop.
     end do LoopThroughSubreachesInCurrentStreamSegment 

     !Calculate percent difference between assumed Downstream flow and calculated Downstream flow after subreach computations (segment loop).
     TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ =&  
          SubreachDownstreamTotalRRFAlongFountainCreek(CurrentSubreach-1)+ &
          SubreachDownstreamNativeFlowAlongFountainCreek(CurrentSubreach-1)
     SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow=SegmentDownstreamTotalGagedFlow- &
          TotalAccountingProgramDownstreamQToCompareToGagedDownstreamQ
     CheckForNonZeroFlowAtDownstreamEndOFountainCreekSegmentment: &
     if (SegmentDownstreamTotalGagedFlow > 0.0) then
          PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest= &
               SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow/SegmentDownstreamTotalGagedFlow*100.
     else
          PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest=0.0
     end if &
     CheckForNonZeroFlowAtDownstreamEndOFountainCreekSegmentment

     !See if difference is within 0.5% criterium. If not converged, loop back into segment computation without resetting/incrementing upstream variables.
     !Note that a final set of subreach iterations is made after segment iterationsconverge. Last iteration is needed so that bank-storage and channel storage
     !values can be written to appropriate files and that downstream/upstream percentages of each reusable return-flow entity can be calculated.
     CheckForFailureToConverge: if (.not. ConvergenceFlag) then
          CheckForMoreThan20Iterations: if (NumberOfIterationsForConvergence >= 20) then
               ConvergenceFlag=.TRUE.
!  No writes to scratch files for TL savings computation.
               write (SCRATCH2FileUnit,1040) CurrentSegmentNumber
          else if (abs(PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest) < 0.1) then
               ConvergenceFlag=.TRUE.
               NumberOfIterationsForConvergence=NumberOfIterationsForConvergence+1
          else if (abs(PCTDiffSegmentGagedAndComputedTotalDownstrmQForConvergenceTest) >= 0.1) then
               SegmentDifferenceComputedDownstreamQCurrentAndLastIteration= &
                    SegmentDifferenceComputedDownstreamQCurrentAndLastIteration+ &
                    SegmentDifferenceBetweenGagedAndComputedTotalDownstreamFlow
               NumberOfIterationsForConvergence=NumberOfIterationsForConvergence+1
          end if CheckForMoreThan20Iterations
          goto 10
     end if CheckForFailureToConverge

!End of segment loop.
end do LoopThroughStreamSegments

!Write out updates to accounts file.
if (FlagForFryArkDiversion) call ApplyRRFReleaseToUsedBalance (FryArkFlowDiversionAlongFountainCreek, &
     UpstreamReleaseInFountainCreekNeededToDeliverFryArkReturnFlow,AmountOfCSFryArkWaterUsed)
if (FlagForExchangeDiversion) call ApplyRRFReleaseToUsedBalance (ExchangeDiversion1AlongFountainCreek, &
     UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow,AmountOfCSExchangeWaterUsed)
if (FlagForRRFDiversion) call ApplyRRFReleaseToUsedBalance (CSRRFDiversion2AlongFountainCreek, &
     UpstreamReleaseInFountainCreekNeededToDeliverReusedReturnFlow,AmountOfCSWaterUsed)
if (FlagForFryArkDiversion .or. FlagForExchangeDiversion .or. FlagForRRFDiversion)  &
     call WriteDiversionAccountData (ACCTSFileUnit,NumberOfFountainCreekDiversionDitches,InputErrorFlag)


!call WriteLossGainDataFromScratchToPermanentFile (TextOutputFileUnit,BankStorageRecoveryFileUnit,SCRATCH1FileUnit,TotalNumberOfNodes,NumberOfReusableReturnFlowsEntered,InputErrorFlag)
call WriteLossGainDataFromScratchToPermanentFile (InputErrorFlag)
if (InputErrorFlag >= 1) return

return

1000 format ('SUBREACH',i2.2)
1010 format (',',i2)
1020 format (' ',i2)
1030 format ('RU return flow ',i3.3,': ', a56)
1040 format ('ICCHK convergence criteria not met after 20 iterations in Fountain Creek segment',i2,' .')

end
!*****************************************************************************
subroutine ApplyRRFReleaseToUsedBalance (DitchDiversion,TransmountainRelease,UsedFromAccountBalance)
!*****************************************************************************
!Subroutine is used to apply required release amount for each type of transmountain diversion to the 'used' balance in the ditch account.

!Definition of local variables:
!-----------------------------------------------------------------------------
!UsedFromAccountBalance = local array for "AmountOfCSFryArkWaterUsed, AmountOfCSExchangeWaterUsed, and AmountOfCSWaterUsed" account values.
!DitchDiversion = local array for transmountain diversion amounts.
!TransmountainRelease = local array for release amounts for transmountain diversions.
!*****************************************************************************
use CommonVariables, &
     only : NumberOfFountainCreekDiversionDitches,FlagThatTurnsOnBalancingAccount
implicit none

real, intent(in) :: DitchDiversion(50),TransmountainRelease(50)
real, intent(inout) :: UsedFromAccountBalance(50)
integer :: CurrentFountainCreekDiversionDitch
real :: ReleasedVolume

!Convert each transmountain release amount to acre-feet and add to "used" array values.
LoopThroughRRFReleasesAndConvertToAcreFeet: do CurrentFountainCreekDiversionDitch=1,NumberOfFountainCreekDiversionDitches
     CheckForRRFDiversionGTZero: if (DitchDiversion(CurrentFountainCreekDiversionDitch) > 0.0) then
          ReleasedVolume=TransmountainRelease(CurrentFountainCreekDiversionDitch)*1.9835
          !If TM diversion/release is from balancing account, do not add to used account balance.
          if (FlagThatTurnsOnBalancingAccount(CurrentFountainCreekDiversionDitch) == 'n') &
               UsedFromAccountBalance(CurrentFountainCreekDiversionDitch)= &
                    UsedFromAccountBalance(CurrentFountainCreekDiversionDitch)+ReleasedVolume
     end if CheckForRRFDiversionGTZero
end do LoopThroughRRFReleasesAndConvertToAcreFeet

return

end

!*****************************************************************************
subroutine CalculateReleasesNeededForSpecifiedExchangeDiversion (TransmountainDiversion, &
     UpstreamReleaseNeededForSpecifiedRRFDiversion,SubreachDownstreamRRF,SubreachRRFLossGain,CurrentSubreach)
!*****************************************************************************
!Subroutine calculate releases required for specified exchange diversion. Calculate by computing RatioSubreachReleaseRequiredForDiversionToDownstreamRRF of exchange diversion to reusable
!flow at bottom of previous subreach, apply RatioSubreachReleaseRequiredForDiversionToDownstreamRRF to trmtn loss/gain in previous subreach, and subtract proportional loss/gain from
!exchange diversion to get amount at top of subreach.

!Definition of local variables:
!-----------------------------------------------------------------------------
!SubreachDiversionLossOrGain = value for transmountain diversion loss/gain in a given subreach. Calculated by multiplying RatioSubreachReleaseRequiredForDiversionToDownstreamRRF by the total loss/gain for
!    the subreach.
!TransmountainDiversion = local array for transmountain diversion amounts from calling subroutine (either dchrdiv, dchxdiv or dchydiv).
!UpstreamReleaseNeededForSpecifiedRRFDiversion = the upstream release required for a specified transmountain diversion, which is equal to the transmountain diversion amount for
!    the ditch minus the loss/gain calculated for the diversion.
!RatioSubreachReleaseRequiredForDiversionToDownstreamRRF = used to calculate the subreach loss/gain for a transmountain diversion; equal to subreach UpstreamReleaseNeededForSpecifiedRRFDiversion divided by reusable return
!    flow at Downstream node of subreach (SubreachDownstreamRRF).
!SubreachDownstreamRRF = local array for the total quantity of reusable return flow at the downstream node of a given subreach.
!SubreachRRFLossGain = local arrray for total loss/gain of reusable return flow in each subreach.
!CurrentSubreach = current subreach number.
!*****************************************************************************
use CommonVariables, &
     only : NumberOfFountainCreekDiversionDitches,NumberOfMonumentCreekSubreaches,FountainCreekDiversionNode
     
implicit none

real, intent(inout) :: UpstreamReleaseNeededForSpecifiedRRFDiversion(50)
real, intent(in) :: TransmountainDiversion(50),SubreachDownstreamRRF(20),SubreachRRFLossGain(20)
integer, intent(in) :: CurrentSubreach
real :: SubreachDiversionLossOrGain,RatioSubreachReleaseRequiredForDiversionToDownstreamRRF
integer :: CurrentDiversion,CurrentUpstreamSubreach

!###########CODE REWRITE: THIS BLOCK OF OLD CODE WAS RE-WRITTEN TO COMBINE TWO LOOPS.  IT WILL TEST FOR IRCH>2 RIGHT BEFORE UPSTREAM LOOPING############
!    if (CurrentSubreach .gt. 2) then
!      do 20 CurrentDiversion=1,NumberOfFountainCreekDiversionDitches
!        if (FountainCreekDiversionNode(CurrentDiversion) .eq. CurrentSubreach+NumberOfMonumentCreekSubreaches) then
!          if (TransmountainDiversion(CurrentDiversion) .gt. 0.0) UpstreamReleaseNeededForSpecifiedRRFDiversion(CurrentDiversion)=TransmountainDiversion(CurrentDiversion)
!          do 10 CurrentUpstreamSubreach=CurrentSubreach-1,2,-1
!            RatioSubreachReleaseRequiredForDiversionToDownstreamRRF= UpstreamReleaseNeededForSpecifiedRRFDiversion(CurrentDiversion)/SubreachDownstreamRRF(CurrentUpstreamSubreach)
!            SubreachDiversionLossOrGain=RatioSubreachReleaseRequiredForDiversionToDownstreamRRF*SubreachRRFLossGain(CurrentUpstreamSubreach)
!            UpstreamReleaseNeededForSpecifiedRRFDiversion(CurrentDiversion)=UpstreamReleaseNeededForSpecifiedRRFDiversion(CurrentDiversion)-SubreachDiversionLossOrGain
! 10       continue
!        end if
! 20   continue
!    else
!      do 30 CurrentDiversion=1,NumberOfFountainCreekDiversionDitches
!        if (FountainCreekDiversionNode(CurrentDiversion) .eq. CurrentSubreach+NumberOfMonumentCreekSubreaches) then
!          if (TransmountainDiversion(CurrentDiversion) .gt. 0.0) UpstreamReleaseNeededForSpecifiedRRFDiversion(CurrentDiversion)=TransmountainDiversion(CurrentDiversion)
!       end if
! 30   continue
!    end if
!###########CODE REWRITE: THIS BLOCK OF OLD CODE WAS RE-WRITTEN TO TEST FOR IRCH>2 RIGHT BEFORE UPSTREAM LOOPING############
!
LoopThroughAllRRFReleases: do CurrentDiversion=1,NumberOfFountainCreekDiversionDitches

     CheckForRRFReleaseOnFountainCreek: if (FountainCreekDiversionNode(CurrentDiversion) == CurrentSubreach + &
          NumberOfMonumentCreekSubreaches) then
          if (TransmountainDiversion(CurrentDiversion) > 0.0) &
               UpstreamReleaseNeededForSpecifiedRRFDiversion(CurrentDiversion)=TransmountainDiversion(CurrentDiversion)
          CheckForNotFirstSubreach: if (CurrentSubreach > 2) then
               LoopUpstreamAndEstimateRatioSEToRRF: do CurrentUpstreamSubreach=CurrentSubreach-1,2,-1
                    RatioSubreachReleaseRequiredForDiversionToDownstreamRRF = &
                         UpstreamReleaseNeededForSpecifiedRRFDiversion(CurrentDiversion)/ &
                         SubreachDownstreamRRF(CurrentUpstreamSubreach)
                    !Apportion RRF loss/gain to SE based on RatioSubreachReleaseRequiredForDiversionToDownstreamRRF.
                    SubreachDiversionLossOrGain = RatioSubreachReleaseRequiredForDiversionToDownstreamRRF * &
                         SubreachRRFLossGain(CurrentUpstreamSubreach) 
                    !Subtract SE flow from RRF flow.
                    UpstreamReleaseNeededForSpecifiedRRFDiversion(CurrentDiversion) = &
                         UpstreamReleaseNeededForSpecifiedRRFDiversion(CurrentDiversion)- &
                         SubreachDiversionLossOrGain  
               end do LoopUpstreamAndEstimateRatioSEToRRF
          end if CheckForNotFirstSubreach
     end if CheckForRRFReleaseOnFountainCreek
end do LoopThroughAllRRFReleases

return

end

!*****************************************************************************
subroutine WriteLossGainDataFromScratchToPermanentFile (InputErrorFlag)
!*****************************************************************************
!Subroutine writes bank storage loss/gain data and channel storage loss/gain data from scratch file to permanent file ("both_recov.fil").

!Definition of local variables:
!-----------------------------------------------------------------------------
!RecoveryFileHeader = character string at beginning of recovery file ("both_recov.fil").
!VariableHeldForScratchFileInputOutput = array of values to be read from scratch and written to permanent files.
!NameOfRRF = local temporary variable for the name of a reusable return flow.
!CurrentSubreach = local temporary variable for the subreach number.
!*****************************************************************************
use CommonVariables, &
     only : BankStorageRecoveryFileUnit,NumberOfReusableReturnFlowsEntered, &
     SCRATCH1FileUnit,TextOutputFileUnit,TotalNumberOfNodes,VariableHeldForScratchFileInputOutput, &
     SubreachBankStorageRecoveryPeriodLengthInDays, iostatus
implicit none

integer, intent(out) :: InputErrorFlag
integer :: TotalNumberOfNodesWritten,CurrentNode,CurrentRRF,CurrentDayInRecoveryPeriod
character :: RecoveryFileHeader*80,CurrentSubreach*12,NameOfRRF*80

      
InputErrorFlag=0
TotalNumberOfNodesWritten=0
rewind (BankStorageRecoveryFileUnit)
read (BankStorageRecoveryFileUnit,1000,iostat=iostatus) RecoveryFileHeader
!# Error reading file
if (iostatus /= 0) then
     write (TextOutputFileUnit,1020) 'In subroutine WriteLossGainDataFromScratchToPermanentFile. Error reading file both_recov.fil'
      InputErrorFlag=1
      return
end if

rewind (BankStorageRecoveryFileUnit)
write (BankStorageRecoveryFileUnit,1000) RecoveryFileHeader
rewind (SCRATCH1FileUnit)

LoopThroughAllNodes: do CurrentNode=1,TotalNumberOfNodes-1
     TotalNumberOfNodesWritten=TotalNumberOfNodesWritten+1
     read (SCRATCH1FileUnit,1010,iostat=iostatus) CurrentSubreach
      if (iostatus /= 0) then
           write (TextOutputFileUnit,1000) &
           'In subroutine WriteLossGainDataFromScratchToPermanentFile. Scratch1 file read Current Subreach'
           InputErrorFlag=1
           return
     end if
     write (BankStorageRecoveryFileUnit,1010) CurrentSubreach
     LoopThroughAllRRFs: do CurrentRRF=1,NumberOfReusableReturnFlowsEntered
          read (SCRATCH1FileUnit,1020,iostat=iostatus) NameOfRRF
            if (iostatus /= 0) then
                write (TextOutputFileUnit,1000) &
                'In subroutine WriteLossGainDataFromScratchToPermanentFile. Scratch1 file read NameOfRRf'
                InputErrorFlag=1
                return
            end if
          write (BankStorageRecoveryFileUnit,1020) NameOfRRF
          call ReinitializeArrays (VariableHeldForScratchFileInputOutput, &
               SubreachBankStorageRecoveryPeriodLengthInDays(CurrentNode)+1)
          read (SCRATCH1FileUnit,*,iostat=iostatus) (VariableHeldForScratchFileInputOutput(CurrentDayInRecoveryPeriod), &
                CurrentDayInRecoveryPeriod=1,SubreachBankStorageRecoveryPeriodLengthInDays(CurrentNode)+1)
            if (iostatus /= 0) then
                write (TextOutputFileUnit,1000) &
                'In subroutine WriteLossGainDataFromScratchToPermanentFile--Scratch1 file read Current Day, Recov Period'
                InputErrorFlag=1
                return
            end if
          call WriteToBankStorageRecoveryFiles (VariableHeldForScratchFileInputOutput, &
               SubreachBankStorageRecoveryPeriodLengthInDays(CurrentNode)+1,BankStorageRecoveryFileUnit)
     end do LoopThroughAllRRFs
     call ReinitializeArrays (VariableHeldForScratchFileInputOutput,2)
     read (SCRATCH1FileUnit,*,iostat=iostatus) (VariableHeldForScratchFileInputOutput(CurrentDayInRecoveryPeriod), &
          CurrentDayInRecoveryPeriod=1,2)
      if (iostatus /= 0) then
          write (TextOutputFileUnit,1000) &
          'In subroutine WriteLossGainDataFromScratchToPermanentFile--Scratch1 file read VariableHeldForScratchfile.'
          InputErrorFlag=1
          return
      end if
     write (BankStorageRecoveryFileUnit,*) (VariableHeldForScratchFileInputOutput(CurrentDayInRecoveryPeriod), &
          CurrentDayInRecoveryPeriod=1,2)
end do LoopThroughAllNodes

if (TotalNumberOfNodesWritten < TotalNumberOfNodes-1) then
     write (TextOutputFileUnit,1000) &
     'In subroutine WriteLossGainDataFromScratchToPermanentFile--scratch file incomplete.'
     InputErrorFlag=1
     return  
end if

rewind (SCRATCH1FileUnit)
rewind (BankStorageRecoveryFileUnit)

return

1000 format (a100)
1010 format (a12)
1020 format (a80)


end
!*****************************************************************************
subroutine ReinitializeArrays (VariablesForReinitialization,NumberOfArrayValues)
!*****************************************************************************
!Subroutine reinitializes array values.

!Definition of local variables:
!-----------------------------------------------------------------------------
!VariablesForReinitialization = array of values to be reinitialized.
!*****************************************************************************

implicit none
real, intent(inout) :: VariablesForReinitialization(61)
integer, intent(in) :: NumberOfArrayValues
integer :: CurrentArrayValue

LoopThroughAllArrayValuesAndSetToZero: do CurrentArrayValue=1,NumberOfArrayValues
     VariablesForReinitialization(CurrentArrayValue)=0.0
end do LoopThroughAllArrayValuesAndSetToZero

return

end

!*****************************************************************************
subroutine WriteToBankStorageRecoveryFiles (WriteValues,NumberOfValues,BankStorageRecoveryFileUnit)
!*****************************************************************************
!Subroutine to write data to recovery files and retain a consistent readable format--5 values per line, e format.

!Definition of local variables:
!-----------------------------------------------------------------------------
!WriteValues = array of values to be written.
!NumberOfValues = number of values.
!*****************************************************************************
 
implicit none
real, intent(in) :: WriteValues(61)
     
integer, intent(in) :: NumberOfValues,BankStorageRecoveryFileUnit
integer :: BeginningValueOnLastLine,NumberOfLinesWrittenToBankStorageFile,IndexForBeginningValue,IndexForEndingValue, &
     CurrentLineNumber,IndexForValue,il




CheckForNumberOfValuesGE6: if (NumberOfValues >= 6) then
     il=NumberOfValues-mod(NumberOfValues,5)+1
     NumberOfLinesWrittenToBankStorageFile=NumberOfValues/5 ! NumberOfLinesWrittenToBankStorageFile is the number of rows required to print all NumberOfValues values, with 5 values printed per row in the loop below
     IndexForBeginningValue=1
     IndexForEndingValue=5
     LoopThroughAllValuesAndWrite5ValuesPerLine: do CurrentLineNumber=1,NumberOfLinesWrittenToBankStorageFile
          write (BankStorageRecoveryFileUnit,1000) (WriteValues(IndexForValue),IndexForValue=IndexForBeginningValue, &
               IndexForEndingValue)
          IndexForBeginningValue=IndexForBeginningValue+5
          IndexForEndingValue=IndexForEndingValue+5
     end do LoopThroughAllValuesAndWrite5ValuesPerLine
     write (BankStorageRecoveryFileUnit,1000) (WriteValues(IndexForValue),IndexForValue=il,NumberOfValues)
else
     write (BankStorageRecoveryFileUnit,1000) (WriteValues(IndexForValue),IndexForValue=1,NumberOfValues)
end if CheckForNumberOfValuesGE6

return

1000 format (5(e15.6))

end

!*****************************************************************************
subroutine WriteTransitLossOutput (InputErrorFlag)
!*****************************************************************************
!Subroutine writes the output report for each day of program run, except the last page, which is written in subroutine ReadAndWriteAccumulatedRRFs.

!Definition of local variables:
!-----------------------------------------------------------------------------
!SegmentNodeName = name of a node, where nodes are located at the bottom of each segment.
!RemarkIdentifier = character string read from SCRATCH2FileUnit file to identify remarks to be written to output report.
!NoDiversionOutputString = character string set to "--" for writing to output report to indicated that output field has no data.
!RRFFootnoteIdentifier = identifier for some output fields to indicate that a footnote is written at end of table to explain output field.
!DiversionFootnoteIdentifier = identifier for some output fields to indicate that a footnote is written at end of table to explain output field.
!StreamGagingStationNumber = station number for main stream gaging station written to output report.
!OutputReportPageNumber = page number for each page of output report.
!SegmentNodeNameFileUnit = file number for file of node names.
!NumberOfNodes = total number of nodes read from file.
!*****************************************************************************
use CommonVariables, &
only : MaximumNumberOfRRFReleases,BalanceAccountFlag,CSVOutputFileUnit,FileWithIOError, &
FlagToComputeNetGainWater,InputFilePathname,InputOrOutputFilename,MaximumNumberOfRRFReleases, &
NumberOfFountainCreekDiversionDitches,MaximumNumberOfRRFReleases,NumberOfFountainCreekGagingStations, &
NumberOfFountainCreekSubreaches,NumberOfMonumentCreekDiversionDitches,NumberOfMonumentCreekGagingStations, &
NumberOfNativeReturnFlows,NumberOfReusableReturnFlowDiversions,NumberOfMonumentCreekSegments, &
NumberOfMonumentCreekSubreaches,NumberOfReusableReturnFlows,NumberOfMonumentCreekSubreaches,NumberOfReusableReturnFlows, &
NumberOfTributaryGagingStations,NumberSubreachesNativeDiversionsGTNativeFlow,ReleaseDate,SCRATCH2FileUnit, &
SumOfFryArkDiversionsForCurrentDay,SumOfNonFryArkDiversionsForCurrentDay, &
TextOutputFileUnit,USGSGagingStationName_Mainstem,GagingStationDischarge_Mainstem, &
MonumentCreekGagingStationNode,FountainCreekGagingStationNode, &
USGSGagingStationNumber_Tributary,USGSGagingStationName_Tributary,USGSGagingStationNumber_Mainstem, &
GagingStationDischarge_Tributary,TributaryGagingStationNode, &
ReleasedNativeReturnFlowName,NativeReturnFlowDischarge,NativeReturnFlowReleaseNode,ReusableReturnFlowEntity, &
TemporaryReusableDiversionDischarge,ReusableReturnFlowReleaseNode,ReleasedReusableReturnFlowName, &
ReleasedReusableReturnFlowName,ReusableDiversionDischarge,ReusableReturnFlowDischarge,ReusableReturnFlowDeliveryNode, &
RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions,MonumentCreekSubreachDitchNumber, &
SubreachUpstreamTotalRRFAlongMonumentCreek,SubreachSumOfRRFReleases,SubreachSumReusableFlowConvertedToNativeFlow, &
SubreachSumOfRRFDiversions,SubreachGainOrLossAllReusableFlowAlongMonumentCreek, &
SubreachGainOrLossReusableFlowAlongMonumentCreek_pct,SubreachGainOrLossAllReusableFlowAlongMonumentCreek, &
SubreachSumReusedDiversionsAlongFountainCreek,FountainCreekSubreachDitchNumber, &
SubreachUpstreamTotalRRFAlongFountainCreek, &
SubreachGainOrLossAllReusableFlowAlongFountainCreek,SubreachGainOrLossReusableFlowAlongFountainCreek_pct, &
SubreachDownstreamTotalRRFAlongFountainCreek,SubreachDownstreamTotalRRFAlongMonumentCreek, &
SubreachGainOrLossReusableFlowAlongMonumentCreek_pct,SubreachSumExchangeDiversionsAlongFountainCreek, &
SubreachDownstreamTotalRRFAlongMonumentCreek, &
SubreachSumOfMeasuredNativeReturnFlows,SubreachSumNativeFlowDiversionsAlongMonumentCreek, &
SubreachGainOrLossNativeFlowAlongMonumentCreek,SubreachGainOrLossNativeFlowAlongMonumentCreek_pct, &
SubreachSumFryArkDiversionsAlongFountainCreek,SubreachDownstreamNativeFlowAlongMonumentCreek, &
SubreachDownstreamNativeFlowAlongFountainCreek, &
SubreachSumNativeFlowDiversionsAlongFountainCreek,SubreachGainOrLossNativeFlowAlongFountainCreek, &
SubreachGainOrLossNativeFlowAlongFountainCreek_pct,SubreachesNativeFlowLessThan0, &
DivertedNativeReturnFlowNameOnMonumentCreek,&
MonumentCreekDiversionNode,NativeFlowDiversionAlongMonumentCreek,DivertedNativeReturnFlowNameOnMonumentCreek, &
NativeFlowDiversionAlongMonumentCreek,DivertedNativeReturnFlowNameOnFountainCreek,FountainCreekDiversionNode, &
FryArkFlowDiversionAlongFountainCreek,UpstreamReleaseInFountainCreekNeededToDeliverFryArkReturnFlow, &
ExchangeDiversion1AlongFountainCreek,FlagRRFDiversionFromBalancingAccount, &
UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow,DivertedNativeReturnFlowNameOnFountainCreek, &
NativeFlowDiversionAlongFountainCreek,CSRRFDiversion2AlongFountainCreek, &
UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow,CSRRFDiversion2AlongFountainCreek, &
DivertedReusableReturnFlowName,ReusableDiversionDischarge,ReusableDiversionDischarge, &
UpstreamReleaseInFountainCreekNeededToDeliverReusedReturnFlow,ReusableReturnFlowDiversionNode, iostatus, &
SubreachUpstreamNativeFlowAlongMonumentCreek, SubreachUpstreamNativeFlowAlongFountainCreek, &
SubreachSumOfGagingStationDischarge_Tributary

implicit none

integer, intent(out) :: InputErrorFlag


integer :: OutputReportPageNumber,SegmentNodeNameFileUnit,NumberOfNodes,InputFilePathnameLength, &
     InputFilenameLength,LoopIndex,TotalNumberOutputLines,GagingStationIndex,CurrentSegmentNode, &
     FountainCreekGagingStationIndex,CurrentRRFDiversion,NumberOfGagingStations, CalcPageBreak
      
real :: SumOfRRFDiversions,TotalGagedFlowAtUpstreamEndOfSegment,TotalGagedFlowAtDownstreamEndOfSegment, & 
     AccumulatedFryArkExchangeAndRRFDiversions
     
character :: SegmentNodeName(500)*42,RemarkIdentifier*5,NoDiversionOutputString*6
character :: RRFFootnoteIdentifier(MaximumNumberOfRRFReleases)*2,DiversionFootnoteIdentifier*2,StreamGagingStationNumber*8


data NoDiversionOutputString /'  --  '/
data RRFFootnoteIdentifier /MaximumNumberOfRRFReleases*'  '/

InputErrorFlag=0
SegmentNodeNameFileUnit=74

!Open file with list of SegmentNodeName names and read for writing to output.
InputFilePathnameLength=len(trim(InputFilePathname))
InputOrOutputFilename = 'nodes.fil'
InputFilenameLength = len(trim(InputOrOutputFilename))
InputOrOutputFilename = InputFilePathname(:InputFilePathnameLength)//InputOrOutputFilename(:InputFilenameLength)
open (unit=SegmentNodeNameFileUnit,file=InputOrOutputFilename,status='old',iostat=iostatus)

FileWithIOError=InputOrOutputFilename
if (iostatus /= 0) then        !File open error
      write (TextOutputFileUnit,1400) 'In subroutine WriteTransitLossOutput, unable to open "nodes.fil" file.'
      InputErrorFlag = 1
      call ReportInputError()
     return
end if


LoopThroughAndReadUpTo500Nodes: do LoopIndex=1,500
     read (SegmentNodeNameFileUnit,1000, iostat=iostatus) SegmentNodeName(LoopIndex)
      if (iostatus < 0) then   !EOF  goto 20
           exit
      else if (iostatus > 0) then   !Read error
          write (TextOutputFileUnit,1400) 'In subroutine WriteTransitLossOutput, read error in "nodes.fil" file.'
           InputErrorFlag = 1
           return
      end if
end do LoopThroughAndReadUpTo500Nodes

20 NumberOfNodes=LoopIndex-1     !Calculate total number of nodes as number of times through loop - 1.

close (SegmentNodeNameFileUnit)

!Set total number of output pages and first page. Removed "npge" variable because value often would be in error. G. Kuhn, 01/2012.
!    npge=7+((NumberOfReusableReturnFlows/30)*2)
!    Krammes if summary is removed, uncomment this
!         change total # of pages since remove summary
!    npge=6+((NumberOfReusableReturnFlows/30))

OutputReportPageNumber=1

!Write title and date to first page (gage discharges and list of SegmentNodeNames).
write (TextOutputFileUnit,1010) ReleaseDate,OutputReportPageNumber
write (CSVOutputFileUnit,*) ReleaseDate

!Write streamflow data to output. Also write list of SegmentNodeNames. Monument Creek stations.
write (TextOutputFileUnit,1020)
write (CSVOutputFileUnit,*) '*** MONUMENT CREEK STATIONS'
TotalNumberOutputLines=1
LoopThroughMonumentCreekStations: do LoopIndex=1,NumberOfMonumentCreekGagingStations-1
     TotalNumberOutputLines=TotalNumberOutputLines+1
     write (TextOutputFileUnit,1030) USGSGagingStationNumber_Mainstem(LoopIndex),USGSGagingStationName_Mainstem(LoopIndex), &
          GagingStationDischarge_Mainstem(LoopIndex), MonumentCreekGagingStationNode(LoopIndex), &
          SegmentNodeName(TotalNumberOutputLines)
     !Output to csv
     write (CSVOutputFileUnit,1031) USGSGagingStationNumber_Mainstem(LoopIndex),USGSGagingStationName_Mainstem(LoopIndex), &
          GagingStationDischarge_Mainstem(LoopIndex),MonumentCreekGagingStationNode(LoopIndex)
     CheckIfNumberOfMonumentStationsGT5: if (NumberOfMonumentCreekGagingStations-1 > 5 .and. mod(LoopIndex,5) == 0) then
          TotalNumberOutputLines=TotalNumberOutputLines+1
          write (TextOutputFileUnit,1040) SegmentNodeName(TotalNumberOutputLines)
     end if CheckIfNumberOfMonumentStationsGT5
end do LoopThroughMonumentCreekStations

!Fountain Creek stations.
GagingStationIndex=NumberOfMonumentCreekGagingStations+NumberOfFountainCreekGagingStations-1
TotalNumberOutputLines=TotalNumberOutputLines+1
write (TextOutputFileUnit,1040) SegmentNodeName(TotalNumberOutputLines)
write (CSVOutputFileUnit,*) '*** FOUNTAIN CREEK STATIONS'
LoopThroughFountainCreekStations: do CurrentSegmentNode=LoopIndex,GagingStationIndex
     FountainCreekGagingStationIndex=CurrentSegmentNode-NumberOfMonumentCreekGagingStations+1
     TotalNumberOutputLines=TotalNumberOutputLines+1
     write (TextOutputFileUnit,1030) USGSGagingStationNumber_Mainstem(CurrentSegmentNode), &
          USGSGagingStationName_Mainstem(CurrentSegmentNode),GagingStationDischarge_Mainstem(CurrentSegmentNode), &
          FountainCreekGagingStationNode(FountainCreekGagingStationIndex),SegmentNodeName(TotalNumberOutputLines)
     !Output to csv
     write (CSVOutputFileUnit,1031) USGSGagingStationNumber_Mainstem(CurrentSegmentNode), &
          USGSGagingStationName_Mainstem(CurrentSegmentNode),GagingStationDischarge_Mainstem(CurrentSegmentNode), &
          MonumentCreekGagingStationNode(FountainCreekGagingStationIndex)
     CheckIfNumberOfFountainStationsGT5: if (mod(CurrentSegmentNode,5) == 0) then
          TotalNumberOutputLines=TotalNumberOutputLines+1
          write (TextOutputFileUnit,1040) SegmentNodeName(TotalNumberOutputLines)
     end if CheckIfNumberOfFountainStationsGT5
end do LoopThroughFountainCreekStations

!Tributary stations.
TotalNumberOutputLines=TotalNumberOutputLines+1
write (TextOutputFileUnit,1040) SegmentNodeName(TotalNumberOutputLines)
write (CSVOutputFileUnit,*) '*** OTHER TRIBUTARY STATIONS'
LoopThroughOtherStations: do LoopIndex=1,NumberOfTributaryGagingStations
     TotalNumberOutputLines=TotalNumberOutputLines+1
     write (TextOutputFileUnit,1030) USGSGagingStationNumber_Tributary(LoopIndex),USGSGagingStationName_Tributary(LoopIndex), &
          GagingStationDischarge_Tributary(LoopIndex), &
          TributaryGagingStationNode(LoopIndex),SegmentNodeName(TotalNumberOutputLines)
     !Output to csv
     write (CSVOutputFileUnit,1031) USGSGagingStationNumber_Tributary(LoopIndex),USGSGagingStationName_Tributary(LoopIndex), &
          GagingStationDischarge_Tributary(LoopIndex),TributaryGagingStationNode(LoopIndex)
     CheckIfNumberOfOtherStationsGT5: if (mod(LoopIndex,5) == 0) then
          TotalNumberOutputLines=TotalNumberOutputLines+1
          write (TextOutputFileUnit,1040) SegmentNodeName(TotalNumberOutputLines)
     end if CheckIfNumberOfOtherStationsGT5
end do LoopThroughOtherStations

!Write measured/reported native return flows data.
write (CSVOutputFileUnit,*) '*** NATIVE RETURN FLOWS'
write (TextOutputFileUnit,1050) SegmentNodeName(TotalNumberOutputLines+1),SegmentNodeName(TotalNumberOutputLines+2), &
     SegmentNodeName(TotalNumberOutputLines+3),SegmentNodeName(TotalNumberOutputLines+4), &
     SegmentNodeName(TotalNumberOutputLines+5),SegmentNodeName(TotalNumberOutputLines+6), &
     SegmentNodeName(TotalNumberOutputLines+7),SegmentNodeName(TotalNumberOutputLines+8)
TotalNumberOutputLines=TotalNumberOutputLines+8
LoopThroughNativeFlows: do LoopIndex=1,NumberOfNativeReturnFlows
     TotalNumberOutputLines=TotalNumberOutputLines+1
     write (TextOutputFileUnit,1060) ReleasedNativeReturnFlowName(LoopIndex),NativeReturnFlowDischarge(LoopIndex), &
          NativeReturnFlowReleaseNode(LoopIndex),SegmentNodeName(TotalNumberOutputLines)
     !Output to csv
     write (CSVOutputFileUnit,1061) ReleasedNativeReturnFlowName(LoopIndex),NativeReturnFlowDischarge(LoopIndex), &
          NativeReturnFlowReleaseNode(LoopIndex)
     CheckIfNumberOfNativeReturnFlowsGT5: if (LoopIndex > 1 .and. mod(LoopIndex-1,5) == 0) then
          TotalNumberOutputLines=TotalNumberOutputLines+1
          write (TextOutputFileUnit,1040) SegmentNodeName(TotalNumberOutputLines)
     end if CheckIfNumberOfNativeReturnFlowsGT5
end do LoopThroughNativeFlows

!Write last few SegmentNodeNames.
TotalNumberOutputLines=TotalNumberOutputLines+1
write (TextOutputFileUnit,1070) SegmentNodeName(TotalNumberOutputLines)
LoopThroughLastFewNodes: do CurrentSegmentNode=TotalNumberOutputLines+1,NumberOfNodes
     write (TextOutputFileUnit,1040) SegmentNodeName(CurrentSegmentNode)
end do LoopThroughLastFewNodes
write (TextOutputFileUnit,1080)

!Write title and date to second page (reusable return flows).
OutputReportPageNumber=OutputReportPageNumber+1
write (TextOutputFileUnit,1090) ReleaseDate,OutputReportPageNumber

!Set values for selected reusable return flow tags.
RRFFootnoteIdentifier(5)='_1' ! Denotes that delivered discharge includes delivered discharge from reusable return flow Nos. 1, 4, and 8.
RRFFootnoteIdentifier(6)='_2' ! Denotes that delivered discharge includes delivered discharge from reusable return flow No. 2.

!Write reusable return flows release data.
write (TextOutputFileUnit,1100)
write (CSVOutputFileUnit,*) '*** REUSABLE RETURN FLOWS'

! Loop through all RRF's 
LoopThroughRRFs: do LoopIndex=1,NumberOfReusableReturnFlows

     !Sum reusable diversions on basis of diversion source.
     SumOfRRFDiversions=0.0
     DiversionFootnoteIdentifier='  '
     LoopThroughRRFDiversions: do CurrentRRFDiversion=1,NumberOfReusableReturnFlowDiversions
          CheckForRRFDiversionAtUsedRRFNode: if (ReusableReturnFlowEntity(CurrentRRFDiversion) == LoopIndex) then
               SumOfRRFDiversions=SumOfRRFDiversions+TemporaryReusableDiversionDischarge(CurrentRRFDiversion)
               !Write comment to output if any reusable diversions are adjusted.'
               ! _# Means that reusable diversion adjusted from input value listed on reusable diversion input summary page.
               if (ReusableDiversionDischarge(CurrentRRFDiversion) > TemporaryReusableDiversionDischarge(CurrentRRFDiversion)) &
                    DiversionFootnoteIdentifier='_#'
          end if CheckForRRFDiversionAtUsedRRFNode
     end do LoopThroughRRFDiversions
                       
      !Skip printed output if return flow at a segment node is set to negative value.
      CheckWhetherToPrint: if (ReusableReturnFlowReleaseNode(LoopIndex) >= 1) then
      
        !Begin new page of reusable return flow data for every 30 entries.
          ! calculate page break
          CalcPageBreak = mod(LoopIndex,30)
          CheckForNumberOfPagesOfRRFs: if (LoopIndex > 1 .and. CalcPageBreak == 1) then
                 OutputReportPageNumber=OutputReportPageNumber+1
                 write (TextOutputFileUnit,1130)
                 write (TextOutputFileUnit,1090) ReleaseDate,OutputReportPageNumber
                 write (TextOutputFileUnit,1100)
           end if CheckForNumberOfPagesOfRRFs
            
           !# Blank line in report after every fifth RRF, if not time for a page break
           CheckToPrintBlankLineAfterEvery5thRRF: if (LoopIndex > 1 .and. mod(LoopIndex-1,5) == 0 .and. CalcPageBreak /= 1) then
                write (TextOutputFileUnit,*)
           end if CheckToPrintBlankLineAfterEvery5thRRF
               
          ! Print Footnote for RRF 5
          PrintFootnote: if (LoopIndex == 5) then
            write (TextOutputFileUnit,1110) LoopIndex,ReleasedReusableReturnFlowName(LoopIndex),& 
                  ReusableReturnFlowReleaseNode(LoopIndex),ReusableReturnFlowDischarge(LoopIndex),-SumOfRRFDiversions, &
                  DiversionFootnoteIdentifier,-SumOfNonFryArkDiversionsForCurrentDay, &
                  ReusableReturnFlowDeliveryNode(LoopIndex),RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(LoopIndex), &
                  RRFFootnoteIdentifier(LoopIndex)
               
            !CSV output RRF5
            write (CSVOutputFileUnit,1111) LoopIndex,ReleasedReusableReturnFlowName(LoopIndex), &
               ReusableReturnFlowReleaseNode(LoopIndex),ReusableReturnFlowDischarge(LoopIndex),-SumOfRRFDiversions, &
               DiversionFootnoteIdentifier,-SumOfNonFryArkDiversionsForCurrentDay, &
               ReusableReturnFlowDeliveryNode(LoopIndex),RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(LoopIndex)
          !End Footnote 5 addition
        
          ! Footnote RRF 6
          else if (LoopIndex == 6) then
              write (TextOutputFileUnit,1110) LoopIndex,ReleasedReusableReturnFlowName(LoopIndex), &
                    ReusableReturnFlowReleaseNode(LoopIndex),ReusableReturnFlowDischarge(LoopIndex),-SumOfRRFDiversions, &
                    DiversionFootnoteIdentifier,-SumOfFryArkDiversionsForCurrentDay, &
                    ReusableReturnFlowDeliveryNode(LoopIndex),RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(LoopIndex), &
                    RRFFootnoteIdentifier(LoopIndex)
                              
                 !CSV output
                 write (CSVOutputFileUnit,1111) LoopIndex,ReleasedReusableReturnFlowName(LoopIndex), &
                       ReusableReturnFlowReleaseNode(LoopIndex),ReusableReturnFlowDischarge(LoopIndex),-SumOfRRFDiversions, &
                       DiversionFootnoteIdentifier,-SumOfFryArkDiversionsForCurrentDay, &
                       ReusableReturnFlowDeliveryNode(LoopIndex),RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(LoopIndex)
          !End Footnote 6 addition

          else
               write (TextOutputFileUnit,1120) LoopIndex,ReleasedReusableReturnFlowName(LoopIndex), &
                    ReusableReturnFlowReleaseNode(LoopIndex),ReusableReturnFlowDischarge(LoopIndex),-SumOfRRFDiversions, &
                    DiversionFootnoteIdentifier,NoDiversionOutputString,ReusableReturnFlowDeliveryNode(LoopIndex), &
                    RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(LoopIndex),RRFFootnoteIdentifier(LoopIndex)

               !CSV output
               write (CSVOutputFileUnit,1121) LoopIndex,ReleasedReusableReturnFlowName(LoopIndex), &
                    ReusableReturnFlowReleaseNode(LoopIndex),ReusableReturnFlowDischarge(LoopIndex),-SumOfRRFDiversions, &
                    DiversionFootnoteIdentifier,NoDiversionOutputString,ReusableReturnFlowDeliveryNode(LoopIndex), &
                    RRFFlowAtDeliveryNodeAfterTransitLossAndDiversions(LoopIndex)
          end if PrintFootnote
      !End output skip.
     end if CheckWhetherToPrint
          
 
end do LoopThroughRRFs

write (TextOutputFileUnit,1130)

!Write title and date to third page (reusable flow summary by subreach).
OutputReportPageNumber=OutputReportPageNumber+1
write (TextOutputFileUnit,1090) ReleaseDate,OutputReportPageNumber

!Write summary of reusable flows, diversions, and reusable gain/loss, by subreach.

!Monument Creek.
write (TextOutputFileUnit,1140)
write (CSVOutputFileUnit,*) '*** SUBREACH SUMMARY REUSABLE RETURN MONUMENT CRK'
LoopThroughSubreachesAndPrintMonumentCreekRRFs: do LoopIndex=1,NumberOfMonumentCreekSubreaches
    if (LoopIndex > 1 .and. mod(LoopIndex-1,5) == 0) write (TextOutputFileUnit,*)
    write (TextOutputFileUnit,1150) LoopIndex,MonumentCreekSubreachDitchNumber(LoopIndex), &
         SubreachUpstreamTotalRRFAlongMonumentCreek(LoopIndex),SubreachSumOfRRFReleases(LoopIndex), &
         -SubreachSumReusableFlowConvertedToNativeFlow(LoopIndex),-SubreachSumOfRRFDiversions(LoopIndex), &
         SubreachGainOrLossAllReusableFlowAlongMonumentCreek(LoopIndex), &
         SubreachGainOrLossReusableFlowAlongMonumentCreek_pct(LoopIndex), &
         SubreachDownstreamTotalRRFAlongMonumentCreek(LoopIndex)
    write (CSVOutputFileUnit,1151) LoopIndex,MonumentCreekSubreachDitchNumber(LoopIndex), &
         SubreachUpstreamTotalRRFAlongMonumentCreek(LoopIndex),SubreachSumOfRRFReleases(LoopIndex), &
         -SubreachSumReusableFlowConvertedToNativeFlow(LoopIndex),-SubreachSumOfRRFDiversions(LoopIndex), &
         SubreachGainOrLossAllReusableFlowAlongMonumentCreek(LoopIndex), &
         SubreachGainOrLossReusableFlowAlongMonumentCreek_pct(LoopIndex), &
         SubreachDownstreamTotalRRFAlongMonumentCreek(LoopIndex)
end do LoopThroughSubreachesAndPrintMonumentCreekRRFs
!
!Fountain Creek.
write (TextOutputFileUnit,1160)
write (CSVOutputFileUnit,*) '*** SUBREACH SUMMARY REUSABLE RETURN FOUNTAIN CRK'
LoopThroughSubreachesAndPrintFountainCreekRRFs: do LoopIndex=1,NumberOfFountainCreekSubreaches
   if (LoopIndex > 1 .and. mod(LoopIndex-1,5) == 0) write (TextOutputFileUnit,*)
   GagingStationIndex=LoopIndex+NumberOfMonumentCreekSubreaches
   AccumulatedFryArkExchangeAndRRFDiversions=SubreachSumFryArkDiversionsAlongFountainCreek(LoopIndex)+ &
        SubreachSumExchangeDiversionsAlongFountainCreek(LoopIndex)+SubreachSumReusedDiversionsAlongFountainCreek(LoopIndex)
   write (TextOutputFileUnit,1170) GagingStationIndex,FountainCreekSubreachDitchNumber(LoopIndex), &
        SubreachUpstreamTotalRRFAlongFountainCreek(LoopIndex),SubreachSumOfRRFReleases(GagingStationIndex), &
        -SubreachSumReusableFlowConvertedToNativeFlow(GagingStationIndex),-SubreachSumOfRRFDiversions(GagingStationIndex), &
        AccumulatedFryArkExchangeAndRRFDiversions,SubreachGainOrLossAllReusableFlowAlongFountainCreek(LoopIndex), &
        SubreachGainOrLossReusableFlowAlongFountainCreek_pct(LoopIndex),SubreachDownstreamTotalRRFAlongFountainCreek(LoopIndex)
   write (CSVOutputFileUnit,1171) GagingStationIndex,FountainCreekSubreachDitchNumber(LoopIndex), &
        SubreachUpstreamTotalRRFAlongFountainCreek(LoopIndex), &
        SubreachSumOfRRFReleases(GagingStationIndex),-SubreachSumReusableFlowConvertedToNativeFlow(GagingStationIndex), &
        -SubreachSumOfRRFDiversions(GagingStationIndex),AccumulatedFryArkExchangeAndRRFDiversions, &
        SubreachGainOrLossAllReusableFlowAlongFountainCreek(LoopIndex), &
        SubreachGainOrLossReusableFlowAlongFountainCreek_pct(LoopIndex),SubreachDownstreamTotalRRFAlongFountainCreek(LoopIndex)
end do LoopThroughSubreachesAndPrintFountainCreekRRFs
      
write (TextOutputFileUnit,1180)
RemarkIdentifier='ITCHK'
call WriteProgramRemarksToOutput (RemarkIdentifier,SCRATCH2FileUnit,TextOutputFileUnit,InputErrorFlag)
if (InputErrorFlag > 0) return

!Write title/date to fourth page (native and total flow summary by subreach).
OutputReportPageNumber=OutputReportPageNumber+1
write (TextOutputFileUnit,1090) ReleaseDate,OutputReportPageNumber

!Write summary of native flows, native gain/loss, and total flow, by subreach.
write (TextOutputFileUnit,1190)
write (CSVOutputFileUnit,*) '*** SUBREACH SUMMARY NATIVE MONUMENT CRK'

!Monument Creek.
NumberOfGagingStations=1
LoopThroughSubreachesAndPrintMonumentCreekNativeFlows: do LoopIndex=1,NumberOfMonumentCreekSubreaches
     if (LoopIndex > 1 .and. mod(LoopIndex-1,5) == 0) write (TextOutputFileUnit,*)
     StreamGagingStationNumber=''
     TotalGagedFlowAtUpstreamEndOfSegment=SubreachUpstreamNativeFlowAlongMonumentCreek(LoopIndex)+ &
          SubreachUpstreamTotalRRFAlongMonumentCreek(LoopIndex)
     TotalGagedFlowAtDownstreamEndOfSegment=SubreachDownstreamNativeFlowAlongMonumentCreek(LoopIndex)+ &
          SubreachDownstreamTotalRRFAlongMonumentCreek(LoopIndex)
     CheckForMonumentSubreachAtGage: if (MonumentCreekGagingStationNode(NumberOfGagingStations) == LoopIndex) then
          StreamGagingStationNumber=USGSGagingStationNumber_Mainstem(NumberOfGagingStations)
          TotalGagedFlowAtUpstreamEndOfSegment=GagingStationDischarge_Mainstem(NumberOfGagingStations)
          NumberOfGagingStations=NumberOfGagingStations+1
      end if CheckForMonumentSubreachAtGage
     write (TextOutputFileUnit,1200) &
          LoopIndex,MonumentCreekSubreachDitchNumber(LoopIndex), &
          SubreachUpstreamNativeFlowAlongMonumentCreek(LoopIndex), &
          SubreachSumOfGagingStationDischarge_Tributary(LoopIndex), &
          SubreachSumOfMeasuredNativeReturnFlows(LoopIndex), &
          SubreachSumReusableFlowConvertedToNativeFlow(LoopIndex), &
          -SubreachSumNativeFlowDiversionsAlongMonumentCreek(LoopIndex), &
          SubreachGainOrLossNativeFlowAlongMonumentCreek(LoopIndex), &
          SubreachGainOrLossNativeFlowAlongMonumentCreek_pct(LoopIndex), &
          SubreachDownstreamNativeFlowAlongMonumentCreek(LoopIndex),StreamGagingStationNumber, &
          TotalGagedFlowAtUpstreamEndOfSegment,TotalGagedFlowAtDownstreamEndOfSegment
          
     write (CSVOutputFileUnit,1201) &
          LoopIndex,MonumentCreekSubreachDitchNumber(LoopIndex), &
          SubreachUpstreamNativeFlowAlongMonumentCreek(LoopIndex), &
          SubreachSumOfGagingStationDischarge_Tributary(LoopIndex), &
          SubreachSumOfMeasuredNativeReturnFlows(LoopIndex), &
          SubreachSumReusableFlowConvertedToNativeFlow(LoopIndex), &
          -SubreachSumNativeFlowDiversionsAlongMonumentCreek(LoopIndex), &
          SubreachGainOrLossNativeFlowAlongMonumentCreek(LoopIndex), &
          SubreachGainOrLossNativeFlowAlongMonumentCreek_pct(LoopIndex), &
          SubreachDownstreamNativeFlowAlongMonumentCreek(LoopIndex),StreamGagingStationNumber, &
          TotalGagedFlowAtUpstreamEndOfSegment,TotalGagedFlowAtDownstreamEndOfSegment
          
end do LoopThroughSubreachesAndPrintMonumentCreekNativeFlows

!Fountain Creek.
write (TextOutputFileUnit,1210)
write (CSVOutputFileUnit,*) '*** SUBREACH SUMMARY NATIVE FOUNTAIN CRK'
NumberOfGagingStations=1
LoopThroughSubreachesAndPrintFountainCreekNativeFlows: do LoopIndex=1,NumberOfFountainCreekSubreaches
     if (LoopIndex > 1 .and. mod(LoopIndex-1,5) == 0) write (TextOutputFileUnit,*)
     StreamGagingStationNumber=''
     TotalGagedFlowAtUpstreamEndOfSegment=SubreachUpstreamNativeFlowAlongFountainCreek(LoopIndex)+ &
          SubreachUpstreamTotalRRFAlongFountainCreek(LoopIndex)
     TotalGagedFlowAtDownstreamEndOfSegment=SubreachDownstreamNativeFlowAlongFountainCreek(LoopIndex)+ &
          SubreachDownstreamTotalRRFAlongFountainCreek(LoopIndex)
     AccumulatedFryArkExchangeAndRRFDiversions=SubreachSumFryArkDiversionsAlongFountainCreek(LoopIndex)+ &
          SubreachSumExchangeDiversionsAlongFountainCreek(LoopIndex)+SubreachSumReusedDiversionsAlongFountainCreek(LoopIndex)
     CheckForFountainSubreachAtGage: if (FountainCreekGagingStationNode(NumberOfGagingStations)== &
          LoopIndex+NumberOfMonumentCreekSubreaches) then
          StreamGagingStationNumber=USGSGagingStationNumber_Mainstem(NumberOfGagingStations+NumberOfMonumentCreekSegments)
          TotalGagedFlowAtUpstreamEndOfSegment=GagingStationDischarge_Mainstem(NumberOfGagingStations+NumberOfMonumentCreekSegments)
          NumberOfGagingStations=NumberOfGagingStations+1
     end if CheckForFountainSubreachAtGage
     GagingStationIndex=LoopIndex+NumberOfMonumentCreekSubreaches
     write (TextOutputFileUnit,1220) GagingStationIndex,FountainCreekSubreachDitchNumber(LoopIndex), &
          SubreachUpstreamNativeFlowAlongFountainCreek(LoopIndex), &
          SubreachSumOfGagingStationDischarge_Tributary(GagingStationIndex), &
          SubreachSumOfMeasuredNativeReturnFlows(GagingStationIndex), &
          SubreachSumReusableFlowConvertedToNativeFlow(GagingStationIndex), &
          -SubreachSumNativeFlowDiversionsAlongFountainCreek(LoopIndex), &
          SubreachGainOrLossNativeFlowAlongFountainCreek(LoopIndex), &
          SubreachGainOrLossNativeFlowAlongFountainCreek_pct(LoopIndex), &
          SubreachDownstreamNativeFlowAlongFountainCreek(LoopIndex), &
          StreamGagingStationNumber,TotalGagedFlowAtUpstreamEndOfSegment,TotalGagedFlowAtDownstreamEndOfSegment
     write (CSVOutputFileUnit,1221) GagingStationIndex,FountainCreekSubreachDitchNumber(LoopIndex), &
          SubreachUpstreamNativeFlowAlongFountainCreek(LoopIndex), &
          SubreachSumOfGagingStationDischarge_Tributary(GagingStationIndex), &
          SubreachSumOfMeasuredNativeReturnFlows(GagingStationIndex), &
          SubreachSumReusableFlowConvertedToNativeFlow(GagingStationIndex), &
          -SubreachSumNativeFlowDiversionsAlongFountainCreek(LoopIndex), &
          SubreachGainOrLossNativeFlowAlongFountainCreek(LoopIndex), &
          SubreachGainOrLossNativeFlowAlongFountainCreek_pct(LoopIndex), &
          SubreachDownstreamNativeFlowAlongFountainCreek(LoopIndex), &
          StreamGagingStationNumber,TotalGagedFlowAtUpstreamEndOfSegment,TotalGagedFlowAtDownstreamEndOfSegment
end do LoopThroughSubreachesAndPrintFountainCreekNativeFlows
write (TextOutputFileUnit,1230)

!Write out subreach numbers where native diversion more than native streamflow.
CheckForNativeDiversionExceedingnativeFlow: if (NumberSubreachesNativeDiversionsGTNativeFlow > 0) then
     write (TextOutputFileUnit,1240) (SubreachesNativeFlowLessThan0(GagingStationIndex), &
          GagingStationIndex=1,NumberSubreachesNativeDiversionsGTNativeFlow)
     write (TextOutputFileUnit,1242)
end if CheckForNativeDiversionExceedingnativeFlow
RemarkIdentifier='ICCHK'
call WriteProgramRemarksToOutput (RemarkIdentifier,SCRATCH2FileUnit,TextOutputFileUnit,InputErrorFlag)
if (InputErrorFlag > 0) return

!Write title and date to fifth page (native and transmountain diversions).
OutputReportPageNumber=OutputReportPageNumber+1
write (TextOutputFileUnit,1090) ReleaseDate,OutputReportPageNumber

!Write native and transmountain diversions & releases to output for each ditch.
write (TextOutputFileUnit,1250)
write (TextOutputFileUnit,1260)

!Monument Creek diversions.
write (CSVOutputFileUnit,*) '*** NATIVE/TM DIVERS MONUMENT CRK'
LoopThroughAndPrintMonumentCreekDiversions: do LoopIndex=1,NumberOfMonumentCreekDiversionDitches
     if (LoopIndex > 1 .and. mod(LoopIndex-1,5) == 0) write (TextOutputFileUnit,*)
     write (TextOutputFileUnit,1270) LoopIndex,DivertedNativeReturnFlowNameOnMonumentCreek(LoopIndex), &
          MonumentCreekDiversionNode(LoopIndex),NativeFlowDiversionAlongMonumentCreek(LoopIndex)
     write (CSVOutputFileUnit,1271) LoopIndex,DivertedNativeReturnFlowNameOnMonumentCreek(LoopIndex), &
          MonumentCreekDiversionNode(LoopIndex),NativeFlowDiversionAlongMonumentCreek(LoopIndex)
end do LoopThroughAndPrintMonumentCreekDiversions

!Fountain Creek diversions.
write (TextOutputFileUnit,1280)
write (CSVOutputFileUnit,*) '*** NATIVE/TM DIVERS FOUNTAIN CRK'
LoopThroughAndPrintFountainCreekDiversions: do LoopIndex=1,NumberOfFountainCreekDiversionDitches
     if (LoopIndex > 1 .and. mod(LoopIndex-1,5) == 0) write (TextOutputFileUnit,*)
          
     write (TextOutputFileUnit,1290) &
          LoopIndex,DivertedNativeReturnFlowNameOnFountainCreek(LoopIndex), &
          FountainCreekDiversionNode(LoopIndex), &
          NativeFlowDiversionAlongFountainCreek(LoopIndex), &
          FryArkFlowDiversionAlongFountainCreek(LoopIndex), &
          UpstreamReleaseInFountainCreekNeededToDeliverFryArkReturnFlow(LoopIndex), &
          ExchangeDiversion1AlongFountainCreek(LoopIndex), &
          FlagRRFDiversionFromBalancingAccount(LoopIndex), &
          UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow(LoopIndex), &
          CSRRFDiversion2AlongFountainCreek(LoopIndex), &
          UpstreamReleaseInFountainCreekNeededToDeliverReusedReturnFlow(LoopIndex)
          
     write (CSVOutputFileUnit,1291) &
          LoopIndex,DivertedNativeReturnFlowNameOnFountainCreek(LoopIndex), &
          FountainCreekDiversionNode(LoopIndex), &
          NativeFlowDiversionAlongFountainCreek(LoopIndex), &
          FryArkFlowDiversionAlongFountainCreek(LoopIndex), &
          UpstreamReleaseInFountainCreekNeededToDeliverFryArkReturnFlow(LoopIndex), &
          ExchangeDiversion1AlongFountainCreek(LoopIndex), &
          UpstreamReleaseInFountainCreekNeededToDeliverExchangeReturnFlow(LoopIndex), &
          CSRRFDiversion2AlongFountainCreek(LoopIndex), &
          UpstreamReleaseInFountainCreekNeededToDeliverReusedReturnFlow(LoopIndex)
          
end do LoopThroughAndPrintFountainCreekDiversions

!Write output note for transmountain diversion data and any comments from program regarding account balances.
write (TextOutputFileUnit,1300)
RemarkIdentifier='IACHK'
call WriteProgramRemarksToOutput (RemarkIdentifier,SCRATCH2FileUnit,TextOutputFileUnit,InputErrorFlag)
if (InputErrorFlag > 0) return
write (TextOutputFileUnit,1310)

!Write title and date to sixth page (reusable diversions).
OutputReportPageNumber=OutputReportPageNumber+1
write (TextOutputFileUnit,1090) ReleaseDate,OutputReportPageNumber

!Write direct diversions/exchanges (take-outs) of reusable return flows.
write (TextOutputFileUnit,1320)
write (CSVOutputFileUnit,*) '*** REUSABLE RETURN OFF-STREAM'
LoopThroughAndPrintOffStreamRRF: do LoopIndex=1,NumberOfReusableReturnFlowDiversions
     if (LoopIndex > 1 .and. mod(LoopIndex-1,5) == 0) write (TextOutputFileUnit,*)
     write (TextOutputFileUnit,1330) LoopIndex,DivertedReusableReturnFlowName(LoopIndex),ReusableDiversionDischarge(LoopIndex), &
          ReusableReturnFlowDiversionNode(LoopIndex),ReusableReturnFlowEntity(LoopIndex)
     write (CSVOutputFileUnit,1331) LoopIndex,DivertedReusableReturnFlowName(LoopIndex),ReusableDiversionDischarge(LoopIndex), &
          ReusableReturnFlowDiversionNode(LoopIndex),ReusableReturnFlowEntity(LoopIndex)
end do LoopThroughAndPrintOffStreamRRF
write (TextOutputFileUnit,1340)

!Compute and write daily and monthly/annual accumulated reusable return flow quantities input to accounting program for each entity.
call ReadAndWriteAccumulatedRRFs (InputErrorFlag,OutputReportPageNumber)
if (InputErrorFlag > 0) return

!If needed, write results of transit loss savings and balancing account computations to output report.
if (FlagToComputeNetGainWater .or. BalanceAccountFlag) call WriteFinalTransitLossEstimatesToOutput ()

!Write user comments to output report.
call WriteUserComments ()
return


1000 format (a42)

!Format statements for page 1--Station, native returns, and node data.
1010 format (5x,'MONUMENT AND FOUNTAIN CREEKS TRANSIT LOSS COMPUTATIONS FOR RELEASE DATE: ',a10,' (Page ',i2,')')
1020 format (/,13x,'DAILY MEAN DISCHARGES AT THE GAGING STATIONS, IN CFS',17x,'NODES ALONG MONUMENT AND FOUNTAIN CREEKS', &
        /,5x,68('-'),7x,44('-'),/,68x,'INPUT',8x,'NODE',3x,'RIVER',/,6x,'NUMBER',17x,'NAME',24x,'DISCHARGE',3x,'NODE',7x, &
        'NUMBER',2x,'MILE',5x,'NODE DESCRIPTION',/,5x,68('-'),7x,44('-'))
1030 format (5x,a8,3x,a40,2x,f8.2,5x,i2,9x,a42)
1031 format (a8,';',a40,';',f8.2,';',i2)
1040 format (82x,a42)
1050 format (5x,68('='),9x,a42,/,82x,a42,/,82x,a42,/,23x,'REPORTED NATIVE RETURN FLOWS, IN CFS',23x,a42 &
        /,5x,72('-'),5x,a42,/,72x,'INPUT',5x,a42,/,29x,'NAME',28x,'DISCHARGE',3x,'NODE',5x,a42,/,5x,72('-'),5x,a42)
1060 format (5x,a56,1x,f8.2,5x,i2,5x,a42)
1061 format (a56,';',f8.2,';',i2)
1070 format (5x,72('='),5x,a42)
1080 format (80x,44('='))

!Format statements for reusable return flow output pages.
1090 format ('',5x,'MONUMENT AND FOUNTAIN CREEKS TRANSIT LOSS COMPUTATIONS FOR RELEASE DATE: ',a10,' (Page ',i2,')')
1100 format (/,29x,'REUSABLE (INCL. TRANSMOUNTAIN) INPUT AND DELIVERD RETURN FLOWS, IN CFS',/,5x,131('-'),/,91x, &
        'REUSABLE',6x,'TRANSMTN',/,91x,'DIVERSNS',6x,'DIVERSNS',2x,/,69x,'INPUT',6x,'INPUT',8x,'FROM',10x,'FROM',4x, &
        'DELIVERY',4x,'DELIVERD',/,5x,'No.',26x,'NAME',32x,'NODE',4x,'DISCHARGE',5x,'ENTITY',8x,'ENTITY',5x, &
        'NODE',6x,'DISCHRG_*',/,5x,131('-'))
1110 format (5x,i3,2x,a56,6x,i2,f12.4,f13.4,a2,f12.4,6x,i2,4x,f10.4,a2)
1111 format (i3,';',a56,';',i2,';',f12.4,';',f13.4,';',a2,';',f12.4,';',i2,';',f10.4)
1120 format (5x,i3,2x,a56,6x,i2,f12.4,f13.4,a2,7x,a6,5x,i2,4x,f10.4,a2)
1121 format (i3,';',a56,';',i2,';',f12.4,';',f13.4,';',a2,';',a6,';',i2,';',f10.4)
1130 format (5x,131('='),/,5x,'NOTES: ','_* Delivered discharge does not include reusable diversion ', &
        'quantities listed to the left.',/,12x,'1. Delivered discharge includes delivered discharge from ', &
        'reusable return flow Nos. 1, 4, and 8.',/,12x,'2. Delivered discharge includes delivered discharge from ', &
        'reusable return flow No. 2.',/,12x,'_# Reusable diversion adjusted from input value ', &
        'listed on reusable diversion input summary page.')

!Format statements for subreach summary of reusable return flows.
1140 format (/,30x,'SUBREACH SUMMARY OF COMPUTATIONS FOR REUSABLE RETURN FLOWS, IN CFS',/,5x,110('-'),/,41x, &
        'NEW',6x,'REUSABLE',36x,'REUSABLE',/,6x,'SUB-',4x,'DITCHES IN',3x,'REUSABLE',4x,'REUSABLE',6x, &
        'TO',6x,'REUSABLE',3x,'TRNSMNTN',3x,'REUSABLE',4x,'G/L, IN',3x,'REUSABLE',/,5x,'REACH',5x,'SUBREACH',5x, &
        'INFLOW',6x,'INPUTS',5x,'NATIVE',4x,'DIVRSION',3x,'DIVRSION',4x,'GN/LS*',4x, &
        'PERCENT*',4x,'OUTFLOW',/,5x,110('-'),/,45x,'M O N U M E N T   C R E E K   R E A C H')
1150 format (6x,i2,1x,a17,f8.2,3(f11.2),8x,'--',1x,2(f11.2),'%',f10.2)
1151 format (i2,';',a17,';',f8.2,';',3(f11.2,';'),2(f11.2,';'),f10.2)
1160 format (/,45x,'F O U N T A I N   C R E E K   R E A C H')
1170 format (6x,i2,1x,a17,f8.2,6(f11.2),'%',f10.2)
1171 format (i2,';',a17,';',f8.2,';',6(f11.2,';'),f10.2)
1180 format (5x,110('='),/,5x,'NOTE: * Reusable gain/loss and percentages computed after subreach adjustments ', &
        'to "REUSABLE INFLOW."')

!Format statements for subreach summary of native and total flows.
1190 format (/,36x,'SUBREACH SUMMARY OF COMPUTATIONS FOR NATIVE FLOWS, IN CFS',/,5x,122('-'),/,54x,'REUSBLE',22x, &
        'NATIVE',9x,' |',2x,'STATION',/,6x,'SUB-',4x,'DITCHES IN',4x,'NATIVE',2x,'TRIBTRY',3x,'NATIVE',5x,'TO',5x, &
        'NATIVE',3x,'NATIVE',3x,'G/L, IN',3x,'NATIVE',' |',2x,'@ INFLW',4x,'TOTAL',3x,'TOTAL',/,5x,'REACH',5x, &
        'SUBREACH',5x,'INFLOW',3x,'INFLOW',2x,'RET_FLW',3x,'NATIVE',2x,'DIVERSN',3x,'GN/LS*',3x,'PERCENT*',2x, &
        'OUTFLW',' |',3x,'NODE',5x,'INFLOW',2x,'OUTFLOW',/,5x,122('-'),/,40x,'M O N U M E N T   C R E E K   R E A C H')
1200 format (6x,i2,1x,a17,f8.2,6(f9.2),'%',f9.2,' |',1x,a8,f9.2,f9.2)
1201 format (i2,';',a17,';',f8.2,';',6(f9.2,';'),f9.2,';',a8,';',f9.2,';',f9.2)
1210 format (/,40x,'F O U N T A I N   C R E E K   R E A C H')
1220 format (6x,i2,1x,a17,f8.2,6(f9.2),'%',f9.2,' |',1x,a8,f9.2,f9.2)
1221 format (i2,';',a17,';',f8.2,';',6(f9.2,';'),f9.2,';',a8,';',f9.2,';',f9.2)
1230 format (5x,122('='),/,5x,'NOTE: * Native gain/loss and percentages computed after subreach adjustments ', &
        'to "NATIVE INFLOW."',/)
1240 format (5x,'Native flow is less than zero (because of native diversions) in subreaches:',20(a3))
1242 format (5x,'Native flow set to zero for T-L calculations; total flow (p. 4) may be less than ', &
        'reusable flow (p. 3) in those subreaches.')

!Format statements for native and transmountain diversions.
1250 format (/,27x,' NATIVE AND TRANSMOUNTAIN DIVERSIONS AND CALCULATED DIVERSION RELEASES*', &
       /,35x,'FOR COLORADO SPRINGS TRANSMOUNTAIN RETURN FLOWS,iN CFS',/,8x,109('-'))
1260 format (58x,'CO.SPGS. FRY-ARK',5x,'CO.SPGS. FRY-ARK',6x,'COLO. SPRINGS',/,60x,'RETURN FLOWS', &
        8x,'FIRST USE EXCH.',7x,'REUSE WATER',/,40x,'DIVRSN',2x,'NATIVE',3x,18('-'),3x,18('-'),3x,18('-'), &
        /,8x,'No.',10x,'DITCH',15x,'NODE',3x,'DIVRSN',4x,'DIVERSN',3x,'RLEASE*',4x,'DIVERSN',3x,'RLEASE*',4x, &
        'DIVERSN',3x,'RLEASE*',/,8x,109('-'),/,48x,'M O N U M E N T   C R E E K   R E A C H')
1270 format (8x,i2,2x,a26,4x,i2,4x,f6.2,6x,'--',8x,'--',9x,'--',8x,'--',9x,'--',8x,'--')
1271 format (i2,';',a26,';',i2,';',f6.2)
1280 format (/,48x,'F O U N T A I N   C R E E K   R E A C H')
1290 format (8x,i2,2x,a26,4x,i2,4x,f6.2,3x,(f6.2,4x,f6.2,5x),(f6.2,a2,2x,f6.2,5x),f6.2,4x,f6.2)
1291 format (i2,';',a26,';',i2,';',f6.2,';',2(f6.2,';',f6.2,';'),f6.2,';',f6.2)
1300 format (8x,109('-'),/,8x,'NOTES: * Transmountain diversion releases calculated in transit loss program.', &
        /,15x,'_B SE Exchange diversion is derived from Balance Account.',/,15x, &
        '_N SE Exchange diversion used to calculate "Net Gain Water."')
1310 format (8x,109('='))

!Format statements for reusable return flow diversion input data and remarks.
1320 format (/,7x,'DIVERSION OF REUSABLE RETURN FLOWS FOR OFF-STREAM STORAGE/EXCHANGE OR OTHER PURPOSES, ', &
        'IN CFS',/,5x,95('-'),/,67x,'DIVERSION',2x,'DIVERSION',2x,'RETURN FLOW',/,5x,'No.',23x,'NAME',32x,'DISCHARGE', &
        4x,'NODE',5x,'SOURCE No.',/,5x,95('-'))
1330 format (5x,i2,2x,a56,1x,f8.2,7x,i2,10x,i2)
1331 format (i2,';',a56,';',f8.2,';',i2,';',i2)
1340 format (5x,95('='))
1400 format (a80,/)

end

!***************************************************************************
subroutine WriteProgramRemarksToOutput (RemarkIdentifier,SCRATCH2FileUnit,TextOutputFileUnit,InputErrorFlag)
!***************************************************************************
!Subroutine writes program-generated remarks to output report.

!Definition of local variables:
!-----------------------------------------------------------------------------
!RemarkIdentifier = character string to identify remark for specific pages of output report.
!Remark = character string for a remark in SCRATCH2FileUnit file.
!***************************************************************************
use CommonVariables, &
     only : iostatus
implicit none

integer, intent(out) :: InputErrorFlag
integer :: TextOutputFileUnit,SCRATCH2FileUnit,CurrentRemark
      
character :: RemarkIdentifier*5,Remark*120

rewind (SCRATCH2FileUnit)
InputErrorFlag = 0

write (TextOutputFileUnit,*)
LoopReadThroughAllLinesInScratch2FileAndWriteToTextOutputFile: do CurrentRemark=1,50
     read (SCRATCH2FileUnit,1000,iostat=iostatus) Remark
      if (iostatus < 0) then   !EOF 
           exit
      else if (iostatus > 0) then   !Read error
          write (TextOutputFileUnit,1000) 'In subroutine WriteProgramRemarksToOutput, read error in "Scratch2" file.'
           InputErrorFlag = 1
           return
      end if
     if (Remark(1:5) == RemarkIdentifier) write (TextOutputFileUnit,1010) Remark(7:120)
end do LoopReadThroughAllLinesInScratch2FileAndWriteToTextOutputFile

999 return

1000 format (a120)
1010 format (5x,a114)

end

!***************************************************************************
subroutine ReadAndWriteAccumulatedRRFs (InputErrorFlag,OutputPageNumber)
!***************************************************************************
!Subroutine reads, updates, and rewrites volume data for the daily, monthly,and annual releases for each individual reusable return flow entity.

!Definition of local variables:
!-----------------------------------------------------------------------------
!WebinputReturnFlowIdentifier = character string in file of monthly and annual reusable return flow volumes to identify the return flow.
!NameOfRRFEntity = local variable for name of reusable return flow entity in file of monthly and annual reusable return flow volumes for comparison to name in
!     input array (ReleasedReusableReturnFlowName).
!OutputWaterYear = output year value to identify current water year.
!SumDailyRRFReleases,SumMonthlyRRFReleases,SumAnnualRRFReleases = the accumulated volumes of the daily, monthly,and annual return flow releases for each entity.
!     monthly and annual reusable return flow volumes are read from file of monthly and annual volumes
!     (ACCUMFileUnit), modified for current day, and re-written.
!PercentDailyFlowForRRFEntity,PercentMonthlyFlowForRRFEntity,PercentAnnualFlowForRRFEntity = the percentages of the daily, monthly, and annual volumes for each entity as a percent of the total
!     reusable return flow volumes.
!DailyRRFEntityUpstreamOfPikeview,MonthlyRRFEntityUpstreamOfPikeview,AnnualRRFEntityUpstreamOfPikeview = the total daily, monthly, and annual reusable return flow volumes for entities upstream from
!      pikefiew station (07104000).
!DailyRRFEntityDownstreamOfPikeview,MonthlyRRFEntityDownstreamOfPikeview,AnnualRRFEntityDownstreamOfPikeview = the total daily, monthly, and annual reusable return flow volumes for entities downstream from
!      pikefiew station (07104000).
!NumberRRFsInLastRun = number of reusable return flows in last run.
!***************************************************************************
use CommonVariables, &
     only : ACCUMFileUnit,CSVOutputFileUnit,DayOfCurrentReleaseDate,MonthOfCurrentReleaseDate, FileWithIOError, &
     NumberOfReusableReturnFlows,ReleaseDate,SCRATCH1FileUnit,SCRATCH2FileUnit,TextOutputFileUnit,YearOfCurrentReleaseDate, &
     ReusableReturnFlowDischarge,ReusableReturnFlowReleaseNode,ReleasedReusableReturnFlowName,iostatus
implicit none

integer, intent(out) :: InputErrorFlag
integer, intent(inout) :: OutputPageNumber
integer :: OutputWaterYear,CurrentRRF,PageCounter,LineCounter

character :: WebinputReturnFlowIdentifier*6,NameOfRRFEntity*56
      

!Variables for daily, monthly, and annual percentages for each entity.
real :: SumDailyRRFReleases,SumMonthlyRRFReleases,SumAnnualRRFReleases,PercentDailyFlowForRRFEntity,PercentMonthlyFlowForRRFEntity,&
     PercentAnnualFlowForRRFEntity

!Variables for total accumulated daily, monthly, and annual volumes for all
!entities upstream and downstream from pikeview station.
real :: DailyRRFEntityUpstreamOfPikeview,MonthlyRRFEntityUpstreamOfPikeview,AnnualRRFEntityUpstreamOfPikeview, &
     DailyRRFEntityDownstreamOfPikeview,MonthlyRRFEntityDownstreamOfPikeview,AnnualRRFEntityDownstreamOfPikeview

rewind (ACCUMFileUnit)
rewind (SCRATCH1FileUnit)
rewind (SCRATCH2FileUnit)
InputErrorFlag=0

!Read reusable return flows accumulation data from last run. If first day of month or year, reset values to zero. Get return flows (array values)
!input for current day, convert to acre-feet, and add to monthly & annual sums. ACCUMFileUnitulate the daily, monthly, and annual sums to a total value;
!separate accumulation for return-flow entities upstream and downstream from pikeview station.

DailyRRFEntityUpstreamOfPikeview=0.0
MonthlyRRFEntityUpstreamOfPikeview=0.0
AnnualRRFEntityUpstreamOfPikeview=0.0
DailyRRFEntityDownstreamOfPikeview=0.0
MonthlyRRFEntityDownstreamOfPikeview=0.0
AnnualRRFEntityDownstreamOfPikeview=0.0

!Read number of reusable return flows in last run.
LoopToReadRRFsFromPreviousDailyRun: do CurrentRRF=1,NumberOfReusableReturnFlows
     SumDailyRRFReleases=0.0
     SumMonthlyRRFReleases=0.0
     SumAnnualRRFReleases=0.0
     read (ACCUMFileUnit,1000,iostat = iostatus) WebinputReturnFlowIdentifier,NameOfRRFEntity,SumMonthlyRRFReleases, &
          SumAnnualRRFReleases
     !# Error on read
     if (iostatus /= 0) then
          write (TextOutputFileUnit,1100) 'In ReadAndWriteAccumulatedRRFs--Error reading "reusable_sum.fil" file.'
         InputErrorFlag=1
           FileWithIOError = 'reusable_sum.fil'
           call ReportInputError ()
         return
     end if 
     if (DayOfCurrentReleaseDate == 1) SumMonthlyRRFReleases=0.0
     ! State water year begins August 1
      if (MonthOfCurrentReleaseDate == 08 .and. DayOfCurrentReleaseDate == 1) SumAnnualRRFReleases=0.0
     SumDailyRRFReleases=ReusableReturnFlowDischarge(CurrentRRF)*1.9835
     SumMonthlyRRFReleases=SumMonthlyRRFReleases+(ReusableReturnFlowDischarge(CurrentRRF)*1.9835)
     SumAnnualRRFReleases=SumAnnualRRFReleases+(ReusableReturnFlowDischarge(CurrentRRF)*1.9835)
     if (ReusableReturnFlowReleaseNode(CurrentRRF) >= 1 .and. ReusableReturnFlowReleaseNode(CurrentRRF) <= 10) then
          DailyRRFEntityUpstreamOfPikeview=DailyRRFEntityUpstreamOfPikeview+SumDailyRRFReleases
          MonthlyRRFEntityUpstreamOfPikeview=MonthlyRRFEntityUpstreamOfPikeview+SumMonthlyRRFReleases
          AnnualRRFEntityUpstreamOfPikeview=AnnualRRFEntityUpstreamOfPikeview+SumAnnualRRFReleases
     else if (ReusableReturnFlowReleaseNode(CurrentRRF) >= 11) then
          DailyRRFEntityDownstreamOfPikeview=DailyRRFEntityDownstreamOfPikeview+SumDailyRRFReleases
          MonthlyRRFEntityDownstreamOfPikeview=MonthlyRRFEntityDownstreamOfPikeview+SumMonthlyRRFReleases
          AnnualRRFEntityDownstreamOfPikeview=AnnualRRFEntityDownstreamOfPikeview+SumAnnualRRFReleases
     end if
     !Write both above and below Pikeview RRF daily, monthly, & annual sums to scratch.
     write (SCRATCH1FileUnit,*) SumDailyRRFReleases,SumMonthlyRRFReleases,SumAnnualRRFReleases
end do LoopToReadRRFsFromPreviousDailyRun

rewind (ACCUMFileUnit)
rewind (SCRATCH1FileUnit)
rewind (SCRATCH2FileUnit)

!Write heading for first page of volume summary output.
OutputPageNumber=OutputPageNumber+1
write (TextOutputFileUnit,1010) ReleaseDate,OutputPageNumber
write (TextOutputFileUnit,1020)
write (TextOutputFileUnit,1030)
write (CSVOutputFileUnit,*) '*** DAILY, MONTHLY, ANNUAL FLOW VOL & PERCENTAGE UPSTREAM OF PIKEVIEW'

!Re-read data in scratch1 file and compute percentages for each entity. If entity is on Monument Creek us from Pikeview station, write to output.
!If entity is Downstream from Pikeview station, write to SCRATCH2FileUnit.
PageCounter=0
LineCounter=0

LoopThroughScratch1AndComputeRRFPercentages: do CurrentRRF=1,NumberOfReusableReturnFlows
     PercentDailyFlowForRRFEntity=0.0
     PercentMonthlyFlowForRRFEntity=0.0
     PercentAnnualFlowForRRFEntity=0.0
     
     CheckToNormalizeUsingTotalRRFVolumeUpstreamFromPikeview: if (ReusableReturnFlowReleaseNode(CurrentRRF) >= 1 .and. &
          ReusableReturnFlowReleaseNode(CurrentRRF) <= 10) then
                  read (SCRATCH1FileUnit,*,iostat=iostatus) SumDailyRRFReleases,SumMonthlyRRFReleases,SumAnnualRRFReleases
                    !# Error on read
                    if (iostatus /= 0) then
                          write (TextOutputFileUnit,1100) 'In ReadAndWriteAccumulatedRRFs--Error reading "Scratch 1" file.'
                          InputErrorFlag=1
                          return
                     end if 
                  
               if (SumDailyRRFReleases > 0.0) PercentDailyFlowForRRFEntity=(SumDailyRRFReleases/ &
                    DailyRRFEntityUpstreamOfPikeview)*100.
               if (SumMonthlyRRFReleases > 0.0) PercentMonthlyFlowForRRFEntity=(SumMonthlyRRFReleases/ &
                    MonthlyRRFEntityUpstreamOfPikeview)*100.
               if (SumAnnualRRFReleases > 0.0) PercentAnnualFlowForRRFEntity=(SumAnnualRRFReleases/ &
                    AnnualRRFEntityUpstreamOfPikeview)*100.
               !Skip printed output if return flow node is set to negative value.
               CheckForUpstreamFromPikeview: if (ReusableReturnFlowReleaseNode(CurrentRRF) >= 1) then 
                      !  Print sums if above Pikeview
                       write (TextOutputFileUnit,1040) &
                              CurrentRRF,ReleasedReusableReturnFlowName(CurrentRRF),SumDailyRRFReleases, &
                              PercentDailyFlowForRRFEntity, SumMonthlyRRFReleases,PercentMonthlyFlowForRRFEntity, &
                              SumAnnualRRFReleases,PercentAnnualFlowForRRFEntity
               
                       write (CSVOutputFileUnit,1041) CurrentRRF,ReleasedReusableReturnFlowName(CurrentRRF), &
                              SumDailyRRFReleases, PercentDailyFlowForRRFEntity,SumMonthlyRRFReleases, &
                              PercentMonthlyFlowForRRFEntity,SumAnnualRRFReleases, PercentAnnualFlowForRRFEntity
                              
                          !If 30 sites have been written, begin new page of output.
                         CheckFor30SitesWritten: if (LineCounter == 30) then
                              OutputPageNumber=OutputPageNumber+1
                              LineCounter=0
                              PageCounter=0
                              write (TextOutputFileUnit,1010) ReleaseDate,OutputPageNumber
                              write (TextOutputFileUnit,1020)
                              write (TextOutputFileUnit,1030)
                       end if CheckFor30SitesWritten
                       if (PageCounter > 0 .and. mod(PageCounter,5) == 0) write (TextOutputFileUnit,*)
                  !End skip.
                  end if CheckForUpstreamFromPikeview
                  
               LineCounter=LineCounter+1
               PageCounter=PageCounter+1
              
          
     else if (ReusableReturnFlowReleaseNode(CurrentRRF) <= 0 .or. ReusableReturnFlowReleaseNode(CurrentRRF) >= 11) then !Normalize Using Total RRF Volume Downstream From Pikeview
          read (SCRATCH1FileUnit,*,iostat=iostatus) SumDailyRRFReleases,SumMonthlyRRFReleases,SumAnnualRRFReleases
          if (iostatus /= 0) then
               write (TextOutputFileUnit,1100) 'In ReadAndWriteAccumulatedRRFs--Error reading "Scratch 1" file.'
               InputErrorFlag=1
               return
          end if 
            
          if (SumDailyRRFReleases > 0.0) &
               PercentDailyFlowForRRFEntity=(SumDailyRRFReleases/DailyRRFEntityDownstreamOfPikeview)*100.
          if (SumMonthlyRRFReleases > 0.0) PercentMonthlyFlowForRRFEntity=(SumMonthlyRRFReleases/ &
               MonthlyRRFEntityDownstreamOfPikeview)*100.
          if (SumAnnualRRFReleases > 0.0) PercentAnnualFlowForRRFEntity=(SumAnnualRRFReleases/ &
               AnnualRRFEntityDownstreamOfPikeview)*100.
          write (SCRATCH2FileUnit,*) SumDailyRRFReleases,PercentDailyFlowForRRFEntity,SumMonthlyRRFReleases, &
               PercentMonthlyFlowForRRFEntity,SumAnnualRRFReleases,PercentAnnualFlowForRRFEntity
     end if CheckToNormalizeUsingTotalRRFVolumeUpstreamFromPikeview
     
     !Write new data computed in previous loop to update 'reusable_sum.fil'.
     write (ACCUMFileUnit,1050) CurrentRRF,ReleasedReusableReturnFlowName(CurrentRRF),SumMonthlyRRFReleases, &
            SumAnnualRRFReleases
     
end do LoopThroughScratch1AndComputeRRFPercentages


!Write total volumes for entities upstream from pikeview station before the downstream stations.
!This assumes that number of return flow entities on Monument Creek
!never would be large enough to completely use 2 pages.

    write (TextOutputFileUnit,1060) DailyRRFEntityUpstreamOfPikeview, &
     MonthlyRRFEntityUpstreamOfPikeview,AnnualRRFEntityUpstreamOfPikeview
       LineCounter=LineCounter+1
      ! Write CSV
      write (CSVOutputFileUnit,1061) DailyRRFEntityUpstreamOfPikeview,MonthlyRRFEntityUpstreamOfPikeview, &
     AnnualRRFEntityUpstreamOfPikeview


rewind (SCRATCH2FileUnit)
PageCounter=0

!Check to see how many sites were written for Monument Creek. If more than 25 sites, begin new page of output for Fountain Creek.
CheckForMoreThan25MonumentCreekRRFs: if (LineCounter > 25) then
     OutputPageNumber=OutputPageNumber+1
     LineCounter=0
     write (TextOutputFileUnit,1010) ReleaseDate,OutputPageNumber
     write (TextOutputFileUnit,1020)
     write (TextOutputFileUnit,1070)
else
     write (TextOutputFileUnit,1070)
end if CheckForMoreThan25MonumentCreekRRFs

write (CSVOutputFileUnit,*) '*** DAILY, MONTHLY, ANN FLOW VOL & PERCENTAGE DOWNSTREAM OF PIKEVIEW'

!Read data for entities downstream from pikeview (SCRATCH2FileUnit) and write to output report.
LoopThroughScratch2DownstreamFromPikeviewAndWriteToOutput: do CurrentRRF=1,NumberOfReusableReturnFlows
     CheckForDownstreamFromPikeview: if (ReusableReturnFlowReleaseNode(CurrentRRF) <= 0 .or. &
          ReusableReturnFlowReleaseNode(CurrentRRF) >= 11) then
          read (SCRATCH2FileUnit,*,iostat = iostatus) SumDailyRRFReleases,PercentDailyFlowForRRFEntity, &
                 SumMonthlyRRFReleases, PercentMonthlyFlowForRRFEntity,SumAnnualRRFReleases,PercentAnnualFlowForRRFEntity
            
            if (iostatus < 0) then   !EOF 
                 exit
            else if (iostatus > 0) then   !Read error
                 write (TextOutputFileUnit,1100) 'In subroutine ReadAndWriteAccumulatedRRFs, read error in "Scratch2" file.'
                 InputErrorFlag = 1
                 return
            end if
                
          !Skip output statements if return flow node is set to negative value.
          SkipDownstreamRRFRecord: if (ReusableReturnFlowReleaseNode(CurrentRRF) >= 1) then
               write (TextOutputFileUnit,1040) CurrentRRF,ReleasedReusableReturnFlowName(CurrentRRF),SumDailyRRFReleases, &
                    PercentDailyFlowForRRFEntity,SumMonthlyRRFReleases,PercentMonthlyFlowForRRFEntity,SumAnnualRRFReleases, &
                    PercentAnnualFlowForRRFEntity
          
                  write (CSVOutputFileUnit,1041) CurrentRRF,ReleasedReusableReturnFlowName(CurrentRRF),SumDailyRRFReleases, &
                       PercentDailyFlowForRRFEntity,SumMonthlyRRFReleases,PercentMonthlyFlowForRRFEntity,SumAnnualRRFReleases, &
                       PercentAnnualFlowForRRFEntity
                 
                  !If 30 sites have been written, begin new page of output.
                 CheckForNewOutputPage: if (LineCounter == 30) then
                       OutputPageNumber=OutputPageNumber+1
                       LineCounter=0
                       PageCounter=0
                       write (TextOutputFileUnit,1010) ReleaseDate,OutputPageNumber
                       write (TextOutputFileUnit,1020)
                       write (TextOutputFileUnit,1070)
                 end if CheckForNewOutputPage
                 ! Write blank line every fifth read
                 if (PageCounter > 0 .and. mod(PageCounter,5) == 0) write (TextOutputFileUnit,*)
            
            ! End skip if flow node is -1.
          end if SkipDownstreamRRFRecord
            
            LineCounter=LineCounter+1
            PageCounter=PageCounter+1
                     
     end if CheckForDownstreamFromPikeview
end do LoopThroughScratch2DownstreamFromPikeviewAndWriteToOutput

60 write (ACCUMFileUnit,1080)

!Set values to define current water year. Krammes - update to new water year months aug/july, was oct/sept.

CheckForWatYearOfExchangeDateOfCurrentReleaseDate: if (MonthOfCurrentReleaseDate >= 8) then
     OutputWaterYear=YearOfCurrentReleaseDate
else if (MonthOfCurrentReleaseDate <= 7) then
     OutputWaterYear=YearOfCurrentReleaseDate-1
end if CheckForWatYearOfExchangeDateOfCurrentReleaseDate

!Write total volumes for entities downstream from pikeview station.
write (TextOutputFileUnit,1090) DailyRRFEntityDownstreamOfPikeview,MonthlyRRFEntityDownstreamOfPikeview, &
     AnnualRRFEntityDownstreamOfPikeview,OutputWaterYear,OutputWaterYear+1
write (CSVOutputFileUnit,1091) DailyRRFEntityDownstreamOfPikeview,MonthlyRRFEntityDownstreamOfPikeview, &
     AnnualRRFEntityDownstreamOfPikeview

return


!accumulated volumes summary pages format statements
1000 format (a6,a56,2(1x,f12.4,1x,f12.4))
1005 format (i4)
1010 format ('',5x,'MONUMENT AND FOUNTAIN CREEKS TRANSIT LOSS COMPUTATIONS FOR RELEASE DATE: ',a10,' (Page ',i2,')')
1020 format (/,12x,'DAILY, MONTHLY, AND ANNUAL REUSABLE RETURN FLOW INPUT VOLUMES AND PERCENTAGES OF TOTAL INPUT VOLUMES', &
        /,5x,121('-'),/,72x,'DAILY VOLUME',5x,'MONTHLY VOLUME',6x,'ANNUAL VOLUME*',/,5x,63x,55('-'),/,5x,'No.',23x,'NAME',34x, &
        'ACRE-FT  PERCENT',4x,'ACRE-FT  PERCENT',4x,'ACRE-FT  PERCENT',/,5x,121('-'))
1030 format (35x,'U P S T R E A M   F R O M   P I K E V I E W   S T A T I O N')
1040 format (5x,i3,2x,a56,3(f12.4,f7.2,'%'))
1041 format (i3,';',a56,3(';',f12.4,';',f7.2))
1050 format ('RR',i3,';',a56,';',f12.4,';',f12.4)
1060 format (5x,121('-'),/,47x,'UPSTREAM TOTALS:',3(f14.4,7x),/)
1061 format ('UPSTREAM TOTALS:',';',3(f14.4,';'))
1070 format (32x,'D O W N S T R E A M   F R O M   P I K E V I E W  S T A T I O N')
1080 format ('END')
1090 format (5x,121('-'),/,45x,'DOWNSTREAM TOTALS:',3(F14.4,7x),/,5x,121('='),/,5x,'NOTES: * Annual volumes are for current ', &
        'water year--August 1, ',i4,' through July 31, ',i4,'.')
1091 format ('DOWNSTREAM TOTALS:',3(';',f14.4))
1100 format (a80,/)

end

!***************************************************************************
subroutine WriteFinalTransitLossEstimatesToOutput ()
!***************************************************************************
!Subroutine makes final computations for results of transit loss savings computations and writes results to output report.

!Definition of local variables:
!-----------------------------------------------------------------------------
!none
!***************************************************************************
use CommonVariables, &
     only : BalanceAccountFlag,FlagToComputeNetGainWater,NumberOfFountainCreekDiversionDitches, &
     TextOutputFileUnit,FlagThatTurnsOnBalancingAccount,ExchangeDiversion1AlongFountainCreek, &
     CSURRFDeliveryWithTransitLossSavings,CSURRFDeliveryWithoutTransitLossSavings
implicit none

integer :: FountainCreekSEExchangeDitch(8),CurrentExchangeDitch,CurrentDiversion
      
real :: TransitLossSavings,SumOfTransitLossDiversions,SumOfBalanceAccountDiversions
data FountainCreekSEExchangeDitch /4,6,7,11,12,15,16,20/

TransitLossSavings=0.0
SumOfTransitLossDiversions=0.0
SumOfBalanceAccountDiversions=0.0
CurrentExchangeDitch=1

!Sum the diversions and releases for the eight qualified ditches.
LoopThroughDiversionsAndReleasesForQualifiedDiversionDitches: do CurrentDiversion=1,NumberOfFountainCreekDiversionDitches
     CheckForQualifiedDitch: if (CurrentDiversion == FountainCreekSEExchangeDitch(CurrentExchangeDitch)) then
          !Check if SE exchange div. is from balancing account and sum diversions and releases not from bal. acct.
          CheckIfNotFromBalancingAccount: if (FlagThatTurnsOnBalancingAccount(CurrentDiversion) == 'n') then
               SumOfTransitLossDiversions=SumOfTransitLossDiversions+ExchangeDiversion1AlongFountainCreek(CurrentDiversion)
               CurrentExchangeDitch=CurrentExchangeDitch+1
          !Sum diversions and releases from balancing account.
          else if (FlagThatTurnsOnBalancingAccount(CurrentDiversion) == 'y') then
               SumOfBalanceAccountDiversions=SumOfBalanceAccountDiversions+ExchangeDiversion1AlongFountainCreek(CurrentDiversion)
               CurrentExchangeDitch=CurrentExchangeDitch+1
          end if CheckIfNotFromBalancingAccount
     end if CheckForQualifiedDitch

     !Include diversions for greenview ditch that are from balancing account.
     if (CurrentDiversion == 25 .and. FlagThatTurnsOnBalancingAccount(CurrentDiversion) == 'y') SumOfBalanceAccountDiversions= &
          SumOfBalanceAccountDiversions+ExchangeDiversion1AlongFountainCreek(CurrentDiversion)
end do LoopThroughDiversionsAndReleasesForQualifiedDiversionDitches

!Transit loss savings is equal to csu transmountain delivery with the qualified SE exchange diversions in use (wtls), minus the csu transmountain
!delivery without the qualified se exchange diversions in use (wotls), plus the total of all "transit loss savings" diversions. If there are only diversions
!from the balancing account, set tl savings to zero.
if (.not. BalanceAccountFlag) TransitLossSavings=0.0

if (FlagToComputeNetGainWater) TransitLossSavings = &
     CSURRFDeliveryWithTransitLossSavings - CSURRFDeliveryWithoutTransitLossSavings+ &
     SumOfTransitLossDiversions

20 write (TextOutputFileUnit,1000)
write (TextOutputFileUnit,1010) CSURRFDeliveryWithTransitLossSavings,CSURRFDeliveryWithoutTransitLossSavings
write (TextOutputFileUnit,1020) SumOfTransitLossDiversions
write (TextOutputFileUnit,1030) TransitLossSavings
write (TextOutputFileUnit,1040) SumOfBalanceAccountDiversions
write (TextOutputFileUnit,1050)

1000 format (//,10x,'NET GAIN WATER (TL savings) AND BALANCE ACCOUNT SUMMARY (in CFS)',/,10x,64('-'))
1010 format (18x,'CSU transmountain delivery with TL savings =',f7.2,/,15x,'CSU transmountain delivery without TL savings =',f7.2)
1020 format (21x,'Qualified Southeast Exchange diversions =',f7.2)
1030 format (14x,'Net Gain Water (TL save) for CSU transmountain =',f7.2)
1040 format (16x,'Requested diversions from Balancing Account. =',f7.2)
1050 format (10x,64('-'))

return

end
!***************************************************************************
subroutine WriteUserComments
!***************************************************************************
!Subroutine writes any input user comments to output report.

!Definition of local variables:
!-----------------------------------------------------------------------------
!none
!***************************************************************************
use CommonVariables, &
     only : UserCommentFromWebInput,ExchangeDate,ReleaseDate,TextOutputFileUnit, MaximumNumberOfLinesUserInput
implicit none
integer :: CurrentRRFRelease

write (TextOutputFileUnit,1000) ReleaseDate,ExchangeDate
LoopThroughAllRRFReleases: do CurrentRRFRelease=1, MaximumNumberOfLinesUserInput

     if (UserCommentFromWebInput(CurrentRRFRelease)(1:2) == 'EN') exit
     if (CurrentRRFRelease == 1 .and. UserCommentFromWebInput(CurrentRRFRelease)(1:1) == '0') then
          write (TextOutputFileUnit,*) '       NONE'
          exit
     else if (UserCommentFromWebInput(CurrentRRFRelease)(1:1) /= '0') then
          write (TextOutputFileUnit,1010) UserCommentFromWebInput(CurrentRRFRelease)
     end if
end do LoopThroughAllRRFReleases

return

1000 format (/,5x,'Remarks to output report for RELEASE DATE: ',a10,', EXCHANGE DATE: ',a10,/)
1010 format (4x,a72)

end

!*****************************************************************************
subroutine CloseFiles
!*****************************************************************************
!Subroutine closes all program files opened in subroutine open.

!Definition of local variables:
!-----------------------------------------------------------------------------
!none
!*****************************************************************************
use CommonVariables, &
     only : WebInputFileUnit,TextOutputFileUnit,CSVOutputFileUnit,LastInputFileUnit,BACK1FileUnit,BACK2FileUnit, &
     ACCTSFileUnit,BankStorageRecoveryFileUnit,SCRATCH1FileUnit,SCRATCH2FileUnit,ACCUMFileUnit
implicit none

close (WebInputFileUnit)
close (TextOutputFileUnit)
close (CSVOutputFileUnit)
close (LastInputFileUnit)
close (BACK1FileUnit)
close (BACK2FileUnit)
close (ACCTSFileUnit)
close (BankStorageRecoveryFileUnit)
close (SCRATCH1FileUnit)
close (SCRATCH2FileUnit)
close (ACCUMFileUnit)

! stop

end

!*****************************************************************************
subroutine ReportInputError()
!*****************************************************************************
!Subroutine reports in the display why an open, read, or write statement failed to execute properly.

!Definition of local variables:
!-----------------------------------------------------------------------------
!none
!*****************************************************************************

!#Positive values of IOStatus indicate error condition, negative IOStatus values indicate EOF or EOR,
!#IOStatus=0 indicates no error. The intrinsic f90_iostat module is supposed to give you error descriptions based
!#the value of IOStatus, but unable to figure out how to access it.

use CommonVariables, &
     only : NumberOfPossibleIOErrors,TextOutputFileUnit,FileWithIOError,iostatus

implicit none
integer CurrentError

character(200) :: IOStatDescription(NumberOfPossibleIOErrors) 
integer :: IOstatcode(NumberOfPossibleIOErrors)

data IOStatCode /-4,-2,-1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31, &
  32,33,34,36,37,38, 39,40,41,42,43,44,45,46,47,48,49,53,56,58,58,84,85,86,87,88,90,91,92,93,94,95,96,97,98,107, &
  110,120,121,122,125,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,144,145,151,152,153,156,159, &
  163,164,165,183,184,186,187,191,192,193,194,195,196,197,198,199/
data IOStatDescription / &
"End of record encountered on a nonadvancing, format-directed READ of an external file.", &
"End of file encountered on READ of an internal file.", &
"End of file encountered on sequential or stream READ of an external file, or END= is specified on a &
&direct access read and the record is nonexistent.", &   
"END= is not specified on a direct access READ and the record is nonexistent.", &
"End of file encountered on WRITE of an internal file.", &
"End of record encountered on an unformatted file.", &
"End of record encountered on a formatted external file using advancing I/O.", &
"End of record encountered on an internal file.", &
"File cannot be found and STATUS='OLD' is specified on an OPEN statement.", &
"Incorrect format of list-directed input found in an external file.", &
"Incorrect format of list-directed input found in an internal file.", &
"List-directed or NAMELIST data item too long for the internal file.", &
"Read error on direct file.", &
"Write error on direct file.", &
"Read error on sequential or stream file.", &
"Write error on sequential or stream file.", &
"Error opening file.", &
"Permanent I/O error encountered on file.", &
"Value of REC= specifier invalid on direct I/O.", &
"I/O statement not allowed on direct file.", &
"Direct I/O statement on an unconnected unit.", &
"Unformatted I/O attempted on formatted file.", &
"Formatted I/O attempted on unformatted file.", &
"Sequential or stream I/O attempted on direct file.", &
"Direct I/O attempted on sequential or stream file.", &
"Attempt to connect a file that is already connected to another unit.", &
"OPEN specifiers do not match the connected file's attributes.", &
"RECL= specifier omitted on an OPEN statement for a direct file.", &
"RECL= specifier on an OPEN statement is negative.", &
"ACCESS= specifier on an OPEN statement is invalid.", &
"FORM= specifier on an OPEN statement is invalid.", &
"STATUS= specifier on an OPEN statement is invalid.", &
"BLANK= specifier on an OPEN statement is invalid.", &
"FILE= specifier on an OPEN or INQUIRE statement is invalid.", &
"STATUS='SCRATCH' and FILE= specifier specified on same OPEN statement.", &
"STATUS='KEEP' specified on CLOSE statement when file was opened with STATUS='SCRATCH'.", &
"Value of STATUS= specifier on CLOSE statement is invalid.", &
"Invalid unit number specified in an I/O statement.", &
"Dynamic memory allocation failure - out of memory.", &
"REWIND error.", &
"ENDFILE error.", &
"BACKSPACE error.", &
"Valid logical input not found in external file.", &
"Valid logical input not found in internal file.", &
"Complex value expected using list-directed or NAMELIST input in external file but not found.", &
"Complex value expected using list-directed or NAMELIST input in internal file but not found.", &
"NAMELIST item name specified with unknown or invalid derived-type component name in NAMELIST input.", &
"NAMELIST item name specified with an invalid substring range in NAMELIST input.", &
"A namelist input item was specified with one or more components of nonzero rank.", &
"A namelist input item specified a zero-sized array.", &
"List-directed or namelist input contained an invalid delimited character string.", &
"Mismatched edit descriptor and item type in formatted I/O.", &
"Invalid digit found in input for B, O or Z format edit descriptors.", &
"Format specification error.", &
"Format specification error.", &
"NAMELIST group header not found in external file.", &
"NAMELIST group header not found in internal file.", &
"Invalid NAMELIST input value found in external file.", &
"Invalid NAMELIST input value found in internal file.", &
"Invalid name found in NAMELIST input.", &
"Invalid character in NAMELIST group or item name in input.", &
"Invalid NAMELIST input syntax.", &
"Invalid subscript list for NAMELIST item in input.", &
"I/O statement not allowed on error unit (unit 0).", &
"Invalid repeat specifier for list-directed or NAMELIST input in external file.", &
"Invalid repeat specifier for list-directed or NAMELIST input in internal file.", &
"Integer overflow in input.", &
"Invalid decimal digit found in input.", &
"Input too long for B, O or Z format edit descriptors.", &
"File exists and STATUS='NEW' was specified on an OPEN statement.", &
"Illegal edit descriptor used with a data item in formatted I/O.", &
"The NLWIDTH setting exceeds the length of a record.", &
"Output length of NAMELIST item name or NAMELIST group name is longer than the maximum record length or the output width &
&specified by the NLWIDTH option.", &
"Incomplete record encountered during direct access READ.", &
"BLANK= specifier given on an OPEN statement for an unformatted file.", &
"POSITION= specifier given on an OPEN statement for a direct file.", &
"POSITION= specifier value on an OPEN statement is invalid.", &
"ACTION= specifier value on an OPEN statement is invalid.", &
"ACTION='READWRITE' specified on an OPEN statement to connect a pipe.", &
"DELIM= specifier given on an OPEN statement for an unformatted file.", &
"DELIM= specifier value on an OPEN statement is invalid.", &
"PAD= specifier given on an OPEN statement for an unformatted file.", &
"PAD= specifier value on an OPEN statement is invalid.", &
"The user program is making calls to an unsupported version of the XL Fortran run-time environment.", &
"ADVANCE= specifier value on a READ statement is invalid.", &
"ADVANCE='NO' is not specified when SIZE= is specified on a READ statement.", &
"ADVANCE='NO' is not specified when EOR= is specified on a READ statement.", &
"I/O operation not permitted on the unit because the file was not opened with an appropriate value for the &
&ACTION= specifier.", &
"Unit is not connected when the I/O statement is attempted. Only for READ, WRITE, PRINT, REWIND, and ENDFILE.", &
"Two ENDFILE statements without an intervening REWIND or BACKSPACE on the unit.", &
"CLOSE error.", &
"INQUIRE error.", &
"READ or WRITE attempted when file is positioned after the endfile record.", &
"The FILE= specifier is missing and the STATUS= specifier does not have a value of 'SCRATCH' on an OPEN statement.", &
"ACCESS='DIRECT' is specified on an OPEN statement for a file that can only be accessed sequentially.", &
"POSITION='REWIND' or POSITION='APPEND' is specified on an OPEN statement and the file is a pipe.", &
"Invalid value for RECL= specifier on an OPEN statement.", &
"External file input could not be flushed because the associated device is not seekable.", &
"Multiple connections to a file located on a non-random access device are not allowed.", &
"Multiple connections with ACTION='WRITE' or ACTION='READWRITE' are not allowed.", &
"The record number of the next record that can be read or written is out of the range of the variable specified with &
&the NEXTREC= specifier of the INQUIRE statement.", &
"The maximum record length for the unit is out of the range of the scalar variable specified with the RECL= specifier &
&in the INQUIRE statement.", &
"The number of bytes of data transmitted is out of the range of the scalar variable specified with the SIZE= or &
&NUM= specifier in the I/O statement.", &
"Unit numbers must be between 0 and 2,147,483,647.", &
"NAMELIST comments are not allowed by the Fortran 90 standard.", &
"The RECL= specifier is specified on an OPEN statement that has ACCESS='STREAM'.", &
"The value of the file position is out of the range of the scalar variable specified with the POS= specifier in &
     &the INQUIRE statement.", &
"The value of the file size is out of the range of the scalar variable specified with the SIZE= specifier in the &
     &INQUIRE statement.", &
"The BACKSPACE statement specifies a unit connected for unformatted stream I/O.", &
"POS= specifier on an I/O statement is less than one.", &
"The stream I/O statement cannot be performed on the unit because the unit is not connected for stream access.", &
"POS= specifier on an I/O statement for a unit connected to a non-seekable file.", &
"Stream I/O statement on an unconnected unit.", &
"STREAM is not a valid value for the ACCESS= specifier on an OPEN statement in Fortran 90 or Fortran 95."/


!Find the IOStat input error message corresponding to the value of iostatus for the current input file
!where an input error occurred.
LoopThroughAllIOStatCodesToFindAMatch: do CurrentError=1,NumberOfPossibleIOErrors
     if (iostatus==IOstatcode(CurrentError)) exit
end do LoopThroughAllIOStatCodesToFindAMatch

write(TextOutputFileUnit,1000) FileWithIOError,IOStatDescription(CurrentError)
write(*,1000) FileWithIOError,IOStatDescription(CurrentError)

1000 format("***INPUT/OUTPUT ERROR***",/,t5,"Name of File with Input/Output Error: ",a30,/,t5,"Description of Error: ",a200)
return

end






