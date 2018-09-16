def strToInt(string):
    res = "1"
    for c in string:
        val = ord(c)
        if val < 10:
            res += "00" + str(val)
        elif val < 100:
            res += "0" + str(val)
        else:
            res += str(val)
    return int(res)

def intToStr(i):
    intStr = str(i)[1:] # remove leading 1
    res = ""
    while intStr != "":
        res += chr(int(intStr[:3]))
        intStr = intStr[3:]
    return res