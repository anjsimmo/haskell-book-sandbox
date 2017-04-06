#!/usr/bin/env python
from b2 import *

# 1.11 Chapter Exercises

# Normal Form or diverge?
exercises_b = r"""
(\x.xxx)
(\z.zz)(\y.yy)
(\x.xxx)z
"""

# Beta Reduce
exercises_c = r"""
(\a.(\b.(\c.cba)))zz(\w.(\v.w))
(\x.(\y.xyy))(\a.a)b
(\y.y)(\x.xx)(\z.zq)
(\z.z)(\z.z)(z.zy)
(\x.(\y.xyy))(\y.y)y
(\a.aa)(\b.ba)c
(\x.(\y.(\z.(yz))))(\x.z)(\x.a)
"""

def do_exercises(exersise_text):
   exercises = filter(lambda eg: len(eg) > 0, exersise_text.split('\n'))
   for i, line in enumerate(exercises):
      print('Q{}:'.format(i+1))
      beta_reduce(line)

print('1.11 - "Normal Form or diverge?" exercies\n')
do_exercises(exercises_b)

# gets beta reduction, exercise 3 onward wrong (renames a free var)
print('1.11 - "Beta Reduce" exercies\n')
do_exercises(exercises_c)

