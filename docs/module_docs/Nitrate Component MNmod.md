# Interface: Nitrate Component (MN) and SHETRAN

The nitrate component MNmod.90 is a module and the interface is with the contaminant transport module CMmod.90. Initial nitrate concentrations are set in CMmod.f90 and nitrate is transported within the CMmod.f90 module. MNmod.f90 controls the nitrate source/sink terms.

At the moment the controlling MN subroutine is mncont within MNmod.90 and this is called from subroutine cmsim within CMmod.90. The output from the MN component (SSS1 and SSS2) is used in subroutine colmsm within CMmod.f90

THE MN component is only called if ISMN is true. ISMN is set to TRUE if there is a filename for line 53 in the rundata file (see below) otherwise it is FALSE.

## Input Arguments - Static:

|      Variable       | Description                                                                                                   |
| :-----------------: | ------------------------------------------------------------------------------------------------------------- |
|         MND         | File unit number for static MN data                                                                           |
|        MNFC         | File unit number for time varying organic matter input data                                                   |
|        MNFN         | File unit number for time varying inorganic matter input data                                                 |
|        MNPR         | File unit number for MN output data                                                                           |
|       MNOUT1        | File unit number for MN extra output (carbon)                                                                 |
|       MNOUT2        | File unit number for MN extra output (nitrogen)                                                               |
|       MNOUTPL       | File unit number for MN plant output                                                                          |
|       NCETOP        | Cell number at the ground surface                                                                             |
|        NCON         | Number of contaminants (at the moment only one is allowed if NITS is being used)                              |
|         NEL         | Number of elements                                                                                            |
|         NLF         | Number of link elements                                                                                       |
|         NS          | Number of soils                                                                                               |
|         NV          | Number of vegetations                                                                                         |
|         NX          | Number of elements in the X direction                                                                         |
|         NY          | Number of elements in the Y direction                                                                         |
|   ICMBK(NLFEE,2)    | Elements number for bank elements                                                                             |            |
| ICMREF(NELEE,4,2:2) | Adjacent elements numbers                                                                                     |
|   ICMXY(NXEE,NY)    | Element numbers for grid elements                                                                             |
|    NCOLMB(NELEE)    | Cell number for the bottom of each grid element                                                               |            |
|     NLYR(NELEE)     | Number of different soil layer categories for each grid element                                               |
|       NRD(NV)       | Number of cells in the rotting zone for each vegetation type. Used to calculate the potential plant uptake of | nitrate    |
|     NVC(NELEE)      | Vegetation type for each element. Used to calculate the potential plant uptake of nitrate                     |
| NLYRBT(NEL,NLYREE)  | Cell number for the bottom cell of each soil category for each grid element                                   |
| NTSOIL(NEL,NLYREE)  | Soil type for each soil category for each grid element                                                        |
|         D0          | Reference dispersion coefficient                                                                              |
|         TIH         | Start time of the simulation (hours)                                                                          |
|        RHOPL        | Plant density. Used to calculate the potential plant uptake of nitrate                                        |
|         Z2          | Reference soil column length                                                                                  |
|   DELONE(NPLTEE)    | Fraction of plant that is involved in the annual growth cycle. Used to calculate the potential plant uptake   | of nitrate |
|     DXQQ(NELEE)     | Element width in the X direction for each grid element                                                        |            |
|     DYQQ(NELEE)     | Element width in the Y direction for each grid element                                                        |
|      VSPOR(NS)      | Porosity for each soil type                                                                                   |
|  DELTAZ(LLEE,NEL)   | Cell heights                                                                                                  |
|      PLAI(NV)       | Plant leaf area index. Used to calculate the potential plant uptake of nitrate                                |
|    RDF(NV,LLEE)     | Root density function. Used to calculate the potential plant uptake of nitrate                                |
|  ZVSNOD(LLEE,NEL)   | Distances between cell nodes                                                                                  |
|        BEXBK        | Specifies if there is a bank simulation                                                                       |
|    LINKNS(NLFEE)    | Orientation of each link element                                                                              |


## Input Arguments - Variable

|       Variable       | Description                                                                                                |
| :------------------: | ---------------------------------------------------------------------------------------------------------- |
|         DTUZ         | Time Step                                                                                                  |
|        UZNOW         | Time through the simulation(Hours)                                                                         |
|       CLAI(NV)       | Canopy leaf area index for each vegetation height. Used to calculate the potential plant uptake of nitrate |
|  CCCC(NEL,NCETOP+1)  | Dynamic region nitrate concentration for each cell                                                         |
|    PNETTO(NELEE)     | Effective rainfall for each grid square                                                                    |
|  SSSS(NEL,NCETOP+1)  | Dead Space region nitrate concentration for each cell                                                      |
|        TA(NV)        | Air temperature                                                                                            |
|  VSPSI(NCETOP,NEL)   | Soil potentials for each cell (dynamically allocated)                                                      |
|  VSTHE(NCETOP,NEL)   | Soil moistures for each cell (dynamically allocated)                                                       |
| VSTHEO(NEL,NCETOP+1) | Soil moistures in each cell at the end of the previous timestep                                            |


## Output Arguments

|      Variable      | Description                                                         |
| :----------------: | ------------------------------------------------------------------- |
| SSS1(NEL,NCETOP+1) | Source/sink term for nitrate in the dynamic region for each cell    |
| SSS2(NEL,NCETOP+1) | Source/sink term for nitrate in the dead space region for each cell |



## Notes


1. The following al\* subroutines located in mod\_load\_filedata.f90 are used in the MN code: 

| Subroutine | Description                                                              |
| :--------: | ------------------------------------------------------------------------ |
|   alallf   | reading of category types for floating point arrays                      |
|   alali    | reading of category types for integer arrays                             |
|   alchk    | check  relationship between subject and object for floating point arrays |
|   alchki   | check  relationship between subject and object for integer arrays        |
|   alintp   | interpolation for nitrate concentrations between depths                  |
|   alredc   | read character string                                                    |
|   alredf   | read floating point data                                                 |
|   alredi   | read integer data                                                        |
|   alredl   | read logical data                                                        |
|   alred2   | file name check                                                          |


2. The following subroutines are also used in the MN code:

|    Subroutine    | Description                                                                     |
| :--------------: | ------------------------------------------------------------------------------- |
|      error       | called if there is an error located in sglobal.f90                              |
| hour\_from\_date | convert year,month,day,hour,minute to hours since 1950. Located in utilsmod.f90 |
|      tridag      | solves tridag matrix for temperature calculation. Located in utilsmod.f90       |


3. Variables with EE added to the end (e.g NELEE, LLEE etc) are the hardcoded values set in sglobal.f90. These are used to set the array sizes. If the values of NELEE and LLEE are too big it causes memory issues. The large arrays (e.g VSTHE) are now dynamically allocated. The rest should also be dynamically allocated.

## Input / Output

These are specified at the end of the rundata file e.g for the Slapton 

```
53: nitrate data --------NITRATE

input\_slap\_mn.txt

54: external carbon input

input\_slap\_mnfc.txt

55: external nitrogen input

input\_slap\_mnfn.txt

56: nitrate plant uptake input

input\_slap\_mnpl.txt

57: nitrate output

output\_slap\_mn.txt

58: nitrate extra output (carbon)

output\_slap\_mn-extraC.txt

59: nitrate extra output (nitrogen)

output\_slap\_mn-extraN.txt

60: nitrate plant output

output\_slap\_mn-plant.txt
```

## Notes

1. See CHANGELOG.md for code change details


