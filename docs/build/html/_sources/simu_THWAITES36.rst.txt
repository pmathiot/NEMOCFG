######################
THWAITES36 simulations
######################

Consideration
=============

Large config
------------

At 1/12 the large domain is:
nx=220 x ny=270 x nz=121 = 7 187 400 pts
=> 64 686 600 pts at 1/36 degree resolution

AMUXL12 being 29 120 700 pts, its x2.2 AMUXL12.L75

TWAIGHTESXL36 = x 6 cost of AMUXL12 (I round below because the more land point compare to the total domain in TWAITESXL36

Medium config
-------------

At 1/12 the large domain is:
nx=220 x ny=160 x nz=90 = 3 168 000 pts
=> 28 512 000 pts at 1/36 degree resolution
=> x1.2 AMUXL12.L75

TWAIGHTESM36 = x 3 cost of AMUXL12 (I round below because the more land point compare to the total domain in TWAITESM36)

Small config
------------

At 1/12 the large domain is:
nx=150 x ny=115 x nz=90 = 1 552 500 pts
=> 13 972 500 pts at 1/36 degree resolution
=> x0.5 AMUXL12.L75

TWAIGTESS36 = x 1 cost of AMUXL12 (I round below because the more land point compare to the total domain in TWAITESM36)

It is likely to be less because of there is much more land vs ocean in THWAITESS36 compare to AMUXL12, ie more processor can be removed.
