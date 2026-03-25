# Formula Plant Nitrogen Uptake

This document defines the equations for plant uptake of nitrogen based on the MPL equations as implemented in Shetran v4.

These equations are run for every plant type in every layer in every SU.

## Variables from SV5

| Variable  | Description                                                 |
| :-------: | ----------------------------------------------------------- |
|    $L$    | Total Leaf Area Index, summed over the canopy (-)           |
| $L_{max}$ | Maximum total leaf area index (-)                           |
| $F_{pl}$  | Fraction of plant type in a grid square (-)                 |
|   $F_r$   | Fraction of plant roots in a SU that are within a layer (-) |
|    $T$    | Layer thickness (m)                                         |
|    $t$    | Time (s)                                                    |
|   $dt$    | Timestep (s)                                                |

Variables read from data files:

| Variable | Description                                                                                                      |
| :------: | ---------------------------------------------------------------------------------------------------------------- |
|  $\rho$  | Density of the growing plant material when it is at its maximum (kg/m2)                                          |
|   $G$    | Growth function which is used to account for the growth of the crop that is consumed by animals (e.g. grass) (-) |

## Hardcoded Data

| Variable | Description                                                                                                                                                                |
| :------: | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
|    N     | Fraction of plant uptake that is nitrogen. Depends on the length of time (tgrow) the plants have been growing for since they were last cropped or died back in the winter. |

| Value of $N$ | Length of growing Season |
| :----------: | ------------------------ |
|    0.022     | if $t_{grow} < 15 days$  |
|    0.017     | if $t_{grow} < 30 days$  |
|    0.015     | if $t_{grow} < 45 days$  |
|    0.012     | if $t_{grow} > 45 days$  |


## Local Variables

| Variable | Description                                      |
| :------: | ------------------------------------------------ |
|   $M$    | Mass of the plant material per unit area (kg/m2) |


## Output variables

| Variable | Description                        |
| :------: | ---------------------------------- |
|   $P$    | Plant uptake of nitrogen (kg/m3/s) |


## Equations

$M = \frac{L}{L_max} * F_pl * G * \rho$

$P = \frac{(M^t-M^{t-1}) * N * F_t}{dt * T}$

Where $M^t$ is the mass of plant material at time $t$.

If $P < 0$ then cropping (or leaf loss for the winter) is assummed to have occurred and set $P = 0$.
