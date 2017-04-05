#!/usr/bin/env python
from b2 import *
ex = "(\\x.y)z"
beta_reduce_all(ex)

ex = "(\\x.x)z"
beta_reduce_all(ex)

ex = "(\\x.(\\y.xy))ab"
beta_reduce_all(ex)

ex = "(\\x.(\\y.(\\z.z)x))a"
beta_reduce_all(ex)

