#!/usr/bin/env python
from b2 import *
ex = "(\\x.y)(z)"
print(beta_reduce_step(ex))

ex = "(\\x.x)(z)"
print(beta_reduce_step(ex))
