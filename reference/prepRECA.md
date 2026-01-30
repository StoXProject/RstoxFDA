# Prepare data for R-ECA

Checks and reformats data in preparation for running with
[`runRECA`](runRECA.md) or [`RunRecaEstimate`](RunRecaEstimate.md) Not
to be confused with [`PrepareRecaEstimate`](PrepareRecaEstimate.md),
which is primarily intended for including in Stox projects.

## Usage

``` r
prepRECA(
  samples,
  landings,
  fixedEffects,
  randomEffects,
  carEffect = NULL,
  neighbours = NULL,
  nFish = NULL,
  ageError = NULL,
  minAge = NULL,
  maxAge = NULL,
  maxLength = NULL,
  lengthResolution = NULL,
  testMax = 1000,
  date = NULL,
  month = NULL,
  quarter = NULL,
  hatchDay = 1,
  interaction = NULL
)
```

## Arguments

- samples:

  data.table() with samples, each row corresponding to one sampled fish.
  Contains columns:

  catchId

  :   Column identifying the catch that the sample was taken from.
      Typically a haul or a landing.

  sampleId

  :   Column identifying the sample. If only one sample is taken for
      each catch. This can be set equal to catchId

  date

  :   POSIXct() Date of catch

  Age

  :   integer() Age of fish

  Length

  :   numeric() Length of fish in cm. Must be complete (no NAs)

  Weight

  :   numeric() Weight of fish in kg. Fish with missing values will not
      be included in Weight-given-length model.

  Otolithtype

  :   integer(), optional, Code identifying stock-classification, may
      contain integers 1,2,4 and 5.

  ...

  :   Additional columns which may be used as covariates as covariates.
      Type of covariate must be sepcified in 'fixedEffects',
      'randomEffects' or 'carEffect'

- landings:

  data.table() with total landings, each row corresponding to one cell.
  Contains columns:

  LiveWeightKG

  :   numeric(). Total landings (Live/Round weight in Kg) for the cell

  ...

  :   Additional columns which may be used as covariates. Covariates in
      landings define each cell. Type of covariate must be sepcified in
      'fixedEffects', 'randomEffects' or 'carEffect'

- fixedEffects:

  character() vector specifying fixed effects. Corresponding columns
  must exists in samples and landings.

- randomEffects:

  character() vector specifying random effects. Corresponding columns
  must exists samples (may also exist in landings).

- carEffect:

  character() specifying a random effect with conditional autoregressive
  coefficient. Corresponding columns must exists samples (may also exist
  in landings).

- neighbours:

  list() specifying the neighbourhood-structure for the carEffect.
  neighbours\[a\] should provide a vector of neighbours to a. May be
  NULL of no carEffect is used.

- nFish:

  data.table() with the columns 'sampleId' and 'count', specifying the
  number of fish in the part of the catch that each sample was taken
  from. Not always needed. See details.

- ageError:

  matrix() specifying the probability of read age (rows), given true age
  (columns). Row and column names specify the ages. If NULL, a unit
  matrix is assumed (No error in age reading).

- minAge:

  lowest age to include in model. If NULL, minimal age in samples is
  used. Age range must match any age error matrix provided (ageError)

- maxAge:

  highest age to include in model. If NULL, maximal age in samples is
  used. Age range must match any age error matrix provided (ageError)

- maxLength:

  longest length to include in model. If NULL, maximal length in samples
  is used.

- lengthResolution:

  desired resolution for length groups. If NULL minimal difference in
  first testMax records are used.

- testMax:

  The largest number of record to inspect for deriving lengthResolution.

- date:

  POSIXct() vector, matching the number of rows in 'landings', date of
  catch, see details.

- month:

  integer() vector, matching the number of rows in 'landings', month of
  catch (1 for January, etc.), see details.

- quarter:

  integer() vector, vector, matching the number of rows in 'landings',
  quarter of catch (1 for Q1, etc.), see details.

- hatchDay:

  integer(), encoding the day of the year when fish is consider to
  transition from one age to the next.

- interaction:

  character vector specifying effects that should be included in
  interaction term. Must correspond to effects specified in parameters
  'fixedEffects', 'randomEffects', or 'carEffect'

  sampleID

  :   Column idenitfying the sample, defined as for 'samples'

  count

  :   Estimated number of fish in the part of the catch the sample was
      taken from

## Value

[`RecaData`](RecaData.md) Data and some data related parameters prepared
for running Reca.

## Details

The cell definition is specified by 'landings'. The type of covariates
are specified in fixedEffects, randomEffects and carEffect. All fixed
effects, as well as any car-effect, must be included in the cell
definition. All covariates must occur in samples.

The parameters 'date', 'month', and 'quarter' are used to set the
temporal resolution for catch at age prediction. Provide exactly one of
these, and set the other ones to NULL. Temporal resolution need not
match any temporal covariate used. One can for example run with month,
even if Quarter is a covariate in the model. Note that resolution is
sensitive to data volume. If you get errors in prediction with E_p(a) =
nan, consider trying with quarter.

neighbours must be symetric, so that b %in% neighbours\[a\], implies a
%in% neighbours\[b\]

nfish is only needed when several samples may be taken from the same
catch. If these are stratified in any way (e.g. pre-sorting by size or
sex), an estimate of strata sizes must be given (column 'count'), for
each sample (column 'sampleId'). If these are replicate samples from the
same selection frame, an estimate of the total catch may be given.

If the column 'Otolithtype' is provided, data is prepared for running
stock-splitting analysis. Support 4 otolithtypes, where type 1 and 2
correspond to one stock, and type 4 and 5 correspond to another review
documentation for `eca.estimate` and `eca.predict`, for information
about how to configure running of this stock splitting, and how to
interpret results.

output GlobalParameters: While outputs AgeLength, WeightLength and
Landings are complete and ready for R-ECA runs. This function populates
the list of GlobalParameters only partially. Run parameters have to be
added afterwards.

## Examples

``` r
 data(catchsamples)
 catchsamples$catchId <- catchsamples$LEid
 catchsamples$sampleId <- catchsamples$SAid
 catchsamples$date <- catchsamples$LEdate
 catchsamples$Metier5 <- catchsamples$LEmetier5

 data(landings)
 landings$LiveWeightKG <- landings$OfficialLandingsWeight
 landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5

 # inspect data
 rEcaDataReport(catchsamples, landings, c("Metier5", "VDencrCode"))
#>     Metier5 LiveWeightKG LiveWeightCumFraction NVDencrCode Ndate Ncatch Nsample
#>      <char>        <int>                 <num>       <num> <num>  <num>   <num>
#>  1: OTB_DEF     39822917             0.4175925           1     1      1       1
#>  2: LLS_DEF     26807800             0.6987055          34    16     34      35
#>  3: SSC_DEF     14299890             0.8486575          29    23     29      29
#>  4:  LX_DEF      9328848             0.9464820           0     0      0       0
#>  5: GNS_DEF      2754654             0.9753680          17     9     17      17
#>  6: LLD_DEF      2063217             0.9970034           0     0      0       0
#>  7: PTB_DEF       118092             0.9982417           0     0      0       0
#>  8: LHM_DEF        83410             0.9991164           0     0      0       0
#>  9: OTM_DEF        63339             0.9997806           0     0      0       0
#> 10: FPO_DEF         9051             0.9998755           0     0      0       0
#> 11:  PS_DEF         5267             0.9999307           0     0      0       0
#> 12: OTT_DEF         5155             0.9999848           0     0      0       0
#> 13: GND_DEF         1202             0.9999974           0     0      0       0
#> 14: MIS_DEF          251             1.0000000           0     0      0       0
#>      Nage Nweight Nlength
#>     <num>   <num>   <num>
#>  1:    29      29      29
#>  2:   962     962     962
#>  3:   721     721     721
#>  4:     0       0       0
#>  5:   338     338     338
#>  6:     0       0       0
#>  7:     0       0       0
#>  8:     0       0       0
#>  9:     0       0       0
#> 10:     0       0       0
#> 11:     0       0       0
#> 12:     0       0       0
#> 13:     0       0       0
#> 14:     0       0       0

 # define sampling frame
 landings <- landings[landings$Metier5 %in% c("GNS_DEF", "LLS_DEF", "LX_DEF", "SSC_DEF"),]
 landings <- landings[landings$Area %in% c("27.2.a.2", "27.1.b"),]
 catchsamples <- catchsamples[catchsamples$Metier5 != "OTB_DEF",]

 # merge gear groups
 landings[landings$Metier5 == "LX_DEF", "Metier5"] <- "LSS_LX_DEF"
 landings[landings$Metier5 == "LLS_DEF", "Metier5"] <- "LSS_LX_DEF"
 catchsamples[catchsamples$Metier5 == "LX_DEF", "Metier5"] <- "LSS_LX_DEF"
 catchsamples[catchsamples$Metier5 == "LLS_DEF", "Metier5"] <- "LSS_LX_DEF"

 # inspect data
 rEcaDataReport(catchsamples, landings, c("Metier5", "VDencrCode"))
#>       Metier5 LiveWeightKG LiveWeightCumFraction NVDencrCode Ndate Ncatch
#>        <char>        <int>                 <num>       <int> <int>  <int>
#> 1: LSS_LX_DEF     31558580             0.7190532          34    16     34
#> 2:    SSC_DEF      9827412             0.9429679          29    23     29
#> 3:    GNS_DEF      2503085             1.0000000          17     9     17
#>    Nsample  Nage Nweight Nlength
#>      <int> <int>   <int>   <int>
#> 1:      35   962     962     962
#> 2:      29   721     721     721
#> 3:      17   338     338     338

 #attempt prepRECA, gives error
 if (FALSE) prepRECA(catchsamples,
   landings,
   c("Metier5"),
   NULL,
   quarter = landings$Quarter) # \dontrun{}

 #get catch count estimates
 meanWeights <- stats::aggregate(list(meanW=catchsamples$Weight),
   by=list(sampleId=catchsamples$sampleId),
   FUN=mean)
 #total weight is unique for sampleID, hence the FUN=mean
 totalWeights <- stats::aggregate(list(totalW=catchsamples$SAtotalWtLive),
   by=list(sampleId=catchsamples$sampleId),
   FUN=mean)
 nFish <- merge(totalWeights, meanWeights)
 nFish$count <- nFish$totalW / nFish$meanW
 nFish <- nFish[,c("sampleId", "count")]

 #prepRECA (produce recaData as in data(recaDataExample))
 recaDataExample <- prepRECA(catchsamples,
   landings,
   c("Metier5"),
   NULL,
   nFish = nFish,
   quarter = landings$Quarter)
```
