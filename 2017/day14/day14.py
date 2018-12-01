from time import time

def reverse(text, repeat):
    knot = list(range(256))
    pos = 0
    skip = 0
    for isntevenused in range(repeat):
         for i in text:
            temp = []
            for j in range(i):
                temp.append(knot[(pos+j) % 256])
            for j in range(i):
                knot[(pos+i-1-j) % 256] = temp[j]
            pos += skip + i
            skip += 1
    return knot

def dense(knot):
    dense = [0]*16
    for i in range(16):
        dense[i] = knot[16*i]
        for m in range(1, 16):
            dense[i] ^= knot[16*i+m]
    return dense

def kh(dense):
    knothash = ''
    for i in dense:
        if len(hex(i)[2:]) == 2:
            knothash += hex(i)[2:]
        else:
            knothash += '0' + hex(i)[2:]
    return knothash


start = time()

def hextobin(hexval):
    thelen = len(hexval)*4
    binval = bin(int(hexval, 16))[2:]
    while ((len(binval)) < thelen):
        binval = '0' + binval
    return binval

marked = [[0 for x in range(128)] for y in range(128)]
frag = []
def colorize(cur_x, cur_y, current_color):
    if cur_x >= 128 or cur_x < 0:
        return False
    if cur_y >= 128 or cur_y < 0:
        return False
    if marked[cur_x][cur_y] != 0:
        return False
    if frag[cur_x][cur_y] == 0:
        marked[cur_x][cur_y] = -1
        return False
    marked[cur_x][cur_y] = current_color
    return (colorize(cur_x + 1, cur_y, current_color) or colorize(cur_x - 1, cur_y, current_color) or colorize(cur_x, cur_y - 1, current_color) or colorize(cur_x, cur_y + 1, current_color))

def colorize_until_filled():
    first_x = -1
    first_y = -1
    num_colors = 0
    while(first_x == -1 and first_y == -1):
        for x in range(128):
            for y in range(128):
                if marked[x][y] == 0:
                    first_x = x
                    first_y = y
                    break
            if first_x != -1:
                break
        if first_x >= 0 and first_y >= 0:
            if frag[first_x][first_y] == 1:
                num_colors += 1
                colorize(first_x, first_y, num_colors)
            else:
                marked[first_x][first_y] = -1
            first_x = -1
            first_y = -1
        else:
            return num_colors

raw_inp = 'ugkiagan'
#raw_inp = 'flqrgnkx'

one_count = 0
for k in range(128):
    text2 = []
    inp = raw_inp + '-' + str(k)
    for i in range(len(inp)):
        text2.append(ord(inp[i]))
    text2 += [17, 31, 73, 47, 23]

    knothash = kh(dense(reverse(text2, 64)))

    bin_str = hextobin(knothash)
    frag += [map((lambda x: int(x)), list(bin_str))]
    #print(bin_str)
    one_count += bin_str.count("1")
print('Found ' + str(one_count) + ' ones and ' + str(colorize_until_filled()) + ' distinct regions.')
print('Completed in ' + str(time() - start) + ' seconds.')
