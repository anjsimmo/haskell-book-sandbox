#!/usr/bin/env python
from b2 import *
ex = "(\\x.y)(z)"
beta_reduce_all(ex)

ex = "(\\x.x)(z)"
beta_reduce_all(ex)

ex = "(\\x.(\\y.z))(a)(b)"
beta_reduce_all(ex)

