#!/usr/bin/env python
from beta import beta_reduce

# 1.11 Chapter Exercises

# Normal Form or diverge?
exercises_b = r"""
(\x.xxx)
(\z.zz)(\y.yy)
(\x.xxx)z
"""

# Beta Reduce
# example 6 modified so that first term doesn't shadow free var
# example 7 modified so that first term doesn't shadow free var
exercises_c = r"""
(\a.(\b.(\c.cba)))zz(\w.(\v.w))
(\x.(\y.xyy))(\a.a)b
(\y.y)(\x.xx)(\z.zq)
(\z.z)(\z.zz)(\z.zy)
(\x.(\y.xyy))(\y.y)y
(\m.mm)(\b.ba)c
(\x.(\y.(\m.xm(ym))))(\x.z)(\x.a)
"""

def do_exercises(exersise_text):
   exercises = filter(lambda eg: len(eg) > 0, exersise_text.split('\n'))
   for i, line in enumerate(exercises):
      print('Q{}:'.format(i+1))
      beta_reduce(line)

print('1.11 - "Normal Form or diverge?" exercises\n')
do_exercises(exercises_b)

print('1.11 - "Beta Reduce" exercises\n')
do_exercises(exercises_c)

