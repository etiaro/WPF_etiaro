import matplotlib.pyplot as plt
import matplotlib.patches as patches
import numpy as np
import re


fig, ax = plt.subplots()

minx = 0
maxx = 1
miny = 0
maxy = 1

if input('prostokat[p]/kolo[k]:') == 'k':
    x = float(input('x:'))
    y = float(input('y:'))
    r = float(input('r:'))
    circle = plt.Circle((x, y), r, color='b', fill=False)
    ax.add_patch(circle)
    
    minx = min(minx, x-r-1)
    maxx = max(maxx, x+r+1)
    miny = min(miny, y-r-1)
    maxy = max(maxy, y+r+1)
    
else:
    x1 = float(input('x1:'))
    y1 = float(input('y1:'))
    x2 = float(input('x2:'))
    y2 = float(input('y2:'))
    rect = patches.Rectangle((x1, y1), x2-x1, y2-y1, linewidth=1, edgecolor='b', facecolor='none')
    ax.add_patch(rect)
    
    minx = min(minx, x1-1)
    maxx = max(maxx, x2+1)
    miny = min(miny, y1-1)
    maxy = max(maxy, y2+1)

print('lokalizacja punktu')
x = float(input('x:')) 
y = float(input('y:'))

points = [(x,y)]

zlozenia = re.sub(r"\s+", "",input('zlozenia(lista z ocamula): '))
zlozenia = zlozenia[1:-1]
zl = []
for s in zlozenia.split(';'):
    s = s[1:-1]
    s = s.split(',')
    s[0] = s[0][1:]
    s[1] = s[1][:-1]
    s[2] = s[2][1:]
    s[3] = s[3][:-1]
    zl.append(((float(s[0]), float(s[1])),(float(s[2]), float(s[3]))))

def side(x1,y1,x2,y2,x,y):
    return np.sign(x1*y2+x2*y+x*y1-x1*y-x2*y1-x*y2)
print(side(2.5,0., 2.5,10., 3.,5.))

def mirrorImage(x1, y1, x2, y2, x, y):
    if x2 == x1: 
        return (2*x1-x, y) 
    a = (y2 - y1) / (x2 - x1)
    b = -1
    c = y1 - a * x1

    temp = -2 * (a * x + b * y + c) / (a * a + b * b)
    x = temp * a + x
    y = temp * b + y
    return (x, y)


for ((x1,y1),(x2,y2)) in reversed(zl):
    npoints = []
    for (x,y) in points:
        if side(x1,y1,x2,y2,x,y) != -1:
            npoints.append(mirrorImage(x1, y1, x2, y2, x, y))
            npoints.append((x,y))
    
    points = npoints
    print(points)



for (x,y) in points:
    plt.scatter(x, y, color='r')
    
    minx = min(minx, x-1)
    maxx = max(maxx, x+1)
    miny = min(miny, y-1)
    maxy = max(maxy, y+1)

plt.xlim([minx, maxx])
plt.ylim([miny, maxy])
ax.set_aspect(1.)
plt.show()
