#! /usr/bin/python3
# pylint: disable=C0103
# pylint: disable=C0301

from enum import Enum
import copy
import sys
import string
import array
import argparse
import csv
import re
import functools
from beeprint import pp

#
# OPTIONS
#

class OPTIONS:
    """ Just have some global variables """
    verbose = 0
    skipErrors = True
    indent = 0
    maxRecurseDepth = 32
    tabWidth = 8
    currFn = ""
    currLn = 0
    listStage1 = True
    listStage2 = True
    markAtLineNo = None

    def __init__(self):
        """ Helper "global" class """
        pass

    @staticmethod
    def debug(level, msg, *args):
        """ Just print `msg`, only if level is smaller/ equal to verbosity level set by command line """
        if level > OPTIONS.verbose:
            return
        print("||", '  ' * int(OPTIONS.indent), msg, end="")
        for arg in args:
            print(arg, end="")
        print()

    @staticmethod
    def debugObject(level, msg, obj):
        """ Just print `msg`, only if level is smaller/ equal to verbosity level set by command line.
        After, `beeprint` will pretty print `obj` """
        if level > OPTIONS.verbose:
            return
        print("||", '  ' * int(OPTIONS.indent), msg, end="")
        pp(obj)

    @staticmethod
    def error(number, msg, *args):
        """ Just print `msg`, only if level is smaller/ equal to verbosity level set by command line """
        print("In %s(%d) E%04d: %s" % (OPTIONS.currFn, OPTIONS.currLn, number, msg), end="")
        for arg in args:
            print(arg, end="")
        if OPTIONS.skipErrors:
            print(" Skipping!", end="")
        print()
        if not OPTIONS.skipErrors:
            sys.exit(number)

    @staticmethod
    def indentInc():
        """ Increases the indentation level of debug messages """
        OPTIONS.indent += 1

    @staticmethod
    def indentDec():
        """ Decreases the indentation level of debug messages """
        OPTIONS.indent = max(0, OPTIONS.indent - 1)

    @staticmethod
    def indentTo(level: int):
        """ Sets the indentation level `0..` of debug messages """
        OPTIONS.indent = max(0, level)

    @staticmethod
    def setFilename(fn):
        OPTIONS.currFn = fn

    @staticmethod
    def setLineNo(n):
        OPTIONS.currLn = n

#
# HELPER
#

class Helper:
    """ little helper class of various static methods """

    @staticmethod
    def unescape(text):
        regex = re.compile(r'\\(\\|[0-7]{1,3}|x.[0-9a-f]?|[\'"abfnrt]|.|$)')
        def replace(m):
            b = m.group(1)
            if len(b) == 0:
                raise ValueError("Invalid character escape: '\\'.")
            i = ord(b[0])
            if i == 120:
                v = int(b[1:], 16)
            elif 48 <= i <= 55:
                v = int(b, 8)
            elif i == 34: return '"'
            elif i == 39: return "'"
            elif i == 92: return '\\'
            elif i == 97: return '\a'
            elif i == 98: return '\b'
            elif i == 102: return '\f'
            elif i == 110: return '\n'
            elif i == 114: return '\r'
            elif i == 116: return '\t'
            else:
                raise ValueError("Invalid character escape: '\\%s'." % b)
            return chr(v)
        result = regex.sub(replace, text)
        return result

    @staticmethod
    def unescapeBinary(text):
        # https://stackoverflow.com/questions/14820429/how-do-i-decodestring-escape-in-python3
        regex = re.compile(b'\\\\(\\\\|[0-7]{1,3}|x.[0-9a-f]?|[\'"abfnrt]|.|$)')
        def replace(m):
            b = m.group(1)
            if len(b) == 0:
                raise ValueError("Invalid character escape: '\\'.")
            i = b[0]
            if i == 120:
                v = int(b[1:], 16)
            elif 48 <= i <= 55:
                v = int(b, 8)
            elif i == 34: return b'"'
            elif i == 39: return b"'"
            elif i == 92: return b'\\'
            elif i == 97: return b'\a'
            elif i == 98: return b'\b'
            elif i == 102: return b'\f'
            elif i == 110: return b'\n'
            elif i == 114: return b'\r'
            elif i == 116: return b'\t'
            else:
                raise ValueError("Invalid character escape: '\\%s'." % str(b))
            return v & 0xff
        result = regex.sub(replace, text.encode('utf-8'))
        return result

    @staticmethod
    def expandTabs(line: str, tw=8):
        """ Expands tabs to multiple of columns """
        if tw < 1:
            return line
        res = ""
        for li in line:
            if li != '\t':
                res += li
                continue
            # tab!
            delta = tw - ((len(res) + tw) % tw)
            res += ' ' * delta
        return res

    @staticmethod
    def hexdump(l: list):
        """ Small list of bytes to hex dump """
        return ' '.join('{:02x}'.format(x) for x in l)

    @staticmethod
    def toTwosComplement(n: int, targetBits=8):
        bm = (1 << targetBits) - 1
        if n >= 0:
            return n
        else:
            return ((~(-n)) & bm)+1

    @staticmethod
    def fromTwosComplement(n: int, targetBits=8):
        if n <= ((1 << (targetBits-1))-1):
            return n
        else:
            return n - (1 << targetBits)

    @staticmethod
    def twosComplementTestCases():

        d = [0, 1, 2, 3, 127, -1, -64, -127, -128]
        for dd in d:
            dc = Helper.toTwosComplement(dd)
            db = Helper.fromTwosComplement(dc)
            print("%4d -> $%4x -> %4d" % (dd, dc, db))

        d = [0, 1, 2, 3, 32767, -1, -64, -32767, -32768]
        for dd in d:
            dc = Helper.toTwosComplement(dd, targetBits=16)
            db = Helper.fromTwosComplement(dc, targetBits=16)
            print("%4d -> $%4x -> %4d" % (dd, dc, db))


#
# OPCODES
#

class OpCodeDef:
    """ Definition of an possible opcode; multiple definitions for same opcodes possible """
    def __init__(self):
        self.opcode = ""
        self.asm = ""
        self.op1 = ""
        self.op2 = ""
        self.am1 = ""
        self.am2 = ""
        self.bitPattern = []
        self.operation = ""
        self.mtstate = map(lambda k: "", range(6*5))

class OpCodeDefList:
    """ List of opcode definitios; holds the whole table, unsorted """
    def __init__(self):
        self.opcodes = [] # array of individual opcode definitions
        self.opcluster = {} # dictionary referencing a list of (above) opcode definitions

    def clear(self):
        self.opcodes.clear()
        self.opcluster.clear()

    def add(self, ocd):
        """ this functio adds an opcode-definition `ocd` BOTH to the individual list 
        of `opcodes` as well of `opcluster`, which groups all ocd's with same opcode  """
        # individual
        self.opcodes.append(ocd)
        # cluster
        cluster = ocd.opcode.strip().upper()
        self.opcluster.setdefault(cluster, [])
        self.opcluster[cluster].append(ocd)

    def findCluster(self, key):
        """ returns a list of `OpCodeDef` instances all featuring the
        same opcode string (like LD -> [ (LD A,3), (LD A,I) ]) """
        key = key.strip().upper()
        if key not in self.opcluster:
            return None
        return self.opcluster[key]

    def readTSV(self, fn):
        """ read a TSV, which is export of Z80 ISA table, sheet ISA """
        self.opcodes.clear()
        # start reading files
        continueBitPattern = None
        with open(fn) as tsvfile:
            tsvreader = csv.reader(tsvfile, delimiter="\t")
            for line in tsvreader:
                # make sure we have enough cols
                while len(line) < 21+6*5:
                    line.append("")

                # continue reading a already created opcode by extending its bitpattern
                if continueBitPattern is not None:
                    bp = ("".join(line[7:15])).strip()
                    if len(bp) != 8:
                        # is not a bitpattern; line HAS TO BE WASTE!
                        continueBitPattern = None
                        continue
                    else:
                        # is a bit pattern, so add it
                        if isinstance(continueBitPattern, OpCodeDef):
                            continueBitPattern.bitPattern.append(bp)
                        continue

                # detect a valid line
                op = line[0].strip()
                asm = line[1].strip()
                if not (len(op) > 0 and len(asm) >= len(op) and asm.lower().startswith(op.lower())):
                    continue
                OPTIONS.debug(4, op, "|", asm)

                # create opcode instance and fill
                opc = OpCodeDef()
                opc.opcode = op
                opc.asm = asm
                opc.op1 = line[2].strip()
                opc.op2 = line[3].strip()
                opc.am1 = line[4].strip()
                opc.am2 = line[5].strip()
                bp = "".join(line[7:15])
                opc.bitPattern.append(bp)
                opc.operation = line[19].strip()
                opc.mtstate = line[21:21+6*5]                

                # add opcode to the collection
                self.add(opc)

                # be prepared for additional bit patterns
                continueBitPattern = opc

        # end for opcodes
        OPTIONS.debugObject(4, "opcodes = ", self.opcodes)
        OPTIONS.debugObject(4, "opcluster = ", self.opcluster)

# 
# SYMS
#

class SymDict:
    """ Dictionary of register symbols. 
    Each dictonary tuple has format: (key, symbol, bitpattern) """
    def __init__(self):
        self.syms = {}

    def clear(self):
        self.syms.clear()

    def add(self, key, symbol, bitpattern):
        self.syms.setdefault(key, [])
        self.syms[key].append((key, symbol, bitpattern))

    def get(self, key):
        """ Returns `None` or a LIST `[ (key, symbol, bitpattern) ]` """
        if not key in self.syms:
            return None
        else:
            return self.syms[key]

    def readTSV(self, fn):
        """ read a TSV, which is export of Z80 ISA table, sheet SYMS """
        self.syms.clear()
        # start reading files
        with open(fn) as tsvfile:
            tsvreader = csv.reader(tsvfile, delimiter="\t")
            for line in tsvreader:
                # make sure we have at least 3 cols
                while len(line) < 3:
                    line.append("")

                # valid entry?
                key = line[0].strip()
                symbol = line[1].strip()
                bitpattern = line[2].strip()

                # some cosmetic?
                #if len(key) > 1:
                #    key = "" + key[0]

                # vaild
                if len(key) > 0 and len(symbol) > 0 and len(bitpattern) > 0:
                    self.add(key, symbol, bitpattern)

        # end for opcodes
        OPTIONS.debugObject(4, "syms = ", self.syms)

#
# LABELS
#

class LabelParseException(Exception):
    """ Raised, whenever a parsing error occurs. String argument of exception
    holds more detailed explanation """
    pass

class Label:
    """ Single label tuple """
    def __init__(self, tag: str, kind: str, expression=None, directValue=None):
        self.tag = tag.strip().upper()
        self.kind = kind
        self.expression = expression
        self.directValue = directValue

class Labels:
    """ This class will handle all labels, which can be resolved to 
    numeric constants and will also provide an handling of complex
    expressions based on arithmetics of these labels """
    def __init__(self):
        self.labels = {}
        self.autonum = 1

    def clear(self):
        self.labels.clear()

    def addOrSet(self, label: Label):
        self.labels[label.tag] = label

    def popIfExist(self, tag: str):
        if tag in self.labels:
            self.labels.pop(tag)

    def generateTagName(self, prefix="L"):
        """ Generate a label name """
        tag = prefix + "%04d" % self.autonum
        self.autonum += 1
        return tag

    def findDirectValue(self, dv: int):
        """ Returns 1st `Label`, if matching `directValue` is found. Else `None` """

        for lb in self.labels.values():
            if lb.directValue == dv:
                return lb
        return None

    def reuseLabelForDirectValueOrGenerateNew(self, dv: int, prefix="L", reuseLabels=True):
        """ If label for directValue `dv` exists and `reuseLabels` is true, return its tag name.
        If not, generate new with `prefix` and return that tag name. """

        tag = ""
        if reuseLabels:
            exLab = self.findDirectValue(dv)
            if exLab is not None:
                tag = exLab.tag
        if len(tag) < 1:
            tag = self.generateTagName(prefix)
            self.addOrSet(Label(tag, 'L', directValue=dv))
        return tag

    def listing(self, labelWidth=8, digitsPerHexAdress=4, digitsPerDeciAdress=5, kinds=" L E "):
        """ Generates a listing of labels """

        # kind info
        kindinfo = {'L': "Label", 'E': 'Equ'}

        # sorted listing
        tags = list(self.labels.keys())
        tags.sort(key=lambda k: (self.labels[k].kind, k))

        for t in tags:
            # access
            if not t in self.labels:
                OPTIONS.error(106, "MISMATCH in label table!")
                continue
            label = self.labels[t]

            # filter
            if kinds.find(label.kind) < 1:
                continue

            # fields
            na = label.tag.ljust(labelWidth)
            if label.directValue is None:
                ha = "?" * digitsPerHexAdress
                da = "?" * digitsPerDeciAdress
            else:
                ha = '{num:{fill}{width}x}'.format(num=label.directValue, fill='0', width=digitsPerHexAdress)
                da = '{num:{fill}{width}d}'.format(num=label.directValue, fill=' ', width=digitsPerDeciAdress)
            ki = ""
            if label.kind in kindinfo:
                ki = kindinfo[label.kind]
            ki = ki.ljust(12)
            nf = ""
            if label.expression is not None:
                nf += "; expr = " + label.expression

            # print
            print(na + " = $" + ha + " = " + da + " is " + ki + nf)

        # Done
        return True

    def tryParseExpression(self, text: str):
        """ implements a minimal numerical in-fix notation parser. 
        More or less an academic excercise. 
        Result is a nested list of tuples `(class, precedence, bag)` """

        # stupidly left to right
        pos = 0
        bag = ""
        bagClass = '?'
        l = []
        while pos < len(text):
            c = text[pos]

            # start immediately an recursion?
            if c == '(':
                # end current bag
                if len(bag) > 0:
                    l.append((bagClass, 0, bag))
                bag = ""
                bagClass = '?'
                # search closing parenthesis
                pos2 = pos
                paraNum = 0
                fail = True
                while True:
                    if text[pos2] == '(':
                        paraNum += 1
                    if text[pos2] == ')':
                        paraNum -= 1
                        if paraNum < 1:
                            fail = False
                            pos2 += 1
                            break
                    pos2 += 1
                    if pos2 >= len(text):
                        fail = True
                        break
                if not fail:
                    subtext = text[pos+1:pos2-1]
                    res = self.tryParseExpression(subtext)
                    l.append(('L', 0, res)) # as an encapsulated list!!
                # set pos after subtext
                pos = pos2
                # loop
                continue

            # classify c?
            cClass = '?'
            if "+-*/%&|^!~<>=¬?:".find(c) >= 0:
                cClass = 'O'
            elif " \t\r\n".find(c) >= 0:
                cClass = 'S'
            else:
                cClass = 'W'

            # change from bagClass to cClass?
            if bagClass != cClass:
                # end current bag
                if len(bag) > 0:
                    l.append((bagClass, 0, bag))
                bag = c
                bagClass = cClass
            else:
                # append to bag
                bag += c

            # go further
            pos += 1

        # end current bag
        if len(bag) > 0:
            l.append((bagClass, 0, bag))

        # Done
        return l

    def checkParsedExpression(self, l: list):
        """ Tries to do a basic check and work over on the structure of an parsed
        expression.
        Returns reworked `l` for success, `None` else. """

        # first filter away the space
        l = list(filter(lambda x: x[0] != 'S', l)) # list() in order to iterate over filter object

        # second check for validity of operators and modify precedence
        ranks = [" ! ~ ¬ ", " * / % ", " & | ^  ", " << >> ", " + - ", " && || ", " == != < > <= >= "]
        ranksjoin = ' '.join(ranks)

        for li in range(len(l)):
            (c, _, b) = l[li]
            if c == 'O':
                # validity
                if ranksjoin.find(' '+b+' ') < 0:
                    # not an valid operator
                    raise LabelParseException("Expression contains invalid operators!")

                # precedence
                for prec in range(len(ranks)):
                    if ranks[prec].find(' '+b+' ') >= 0:
                        l[li] = (c, 1+prec, b)

        # third check, that the list alternates between non-operators and operators
        # TODO: does not work for unary operators!!!!!        
        # for i in range(len(l)):
        #     if (i % 2) == 0 && l[i][0] == 'O':
        #         OPTIONS.error(110, "Expression features as element #%d an operator!" % (i+1))
        #         return False
        #     if (i % 2) == 1 && l[i][0] != 'O':
        #         OPTIONS.error(110, "Expression features as element #%d NOT an operator!" % (i+1))
        #         return False

        # go into recursion
        for li in range(len(l)):
            if l[li][0] == 'L':
                sl = self.checkParsedExpression(l[li][2])
                if sl is None:
                    return None
                # replace sub list
                l[li] = ('L', 0, sl)

        # Done
        return l
        
    def evaluateParsedExpression(self, l: list, targetBits=16, recurseDepth=0):
        """ Travels along the expression and tries to integrate/ calculate it.
        Returns `int` if success, `None` else. """

        # trivial
        if len(l) < 1:
            return 0

        # DEBUG
        OPTIONS.debug(3, "evaluateParsedExpression [%s]" % ' '.join(map(lambda x: x[2] if type(x[2]) is not list else "()", l)))
        OPTIONS.indentInc()

        # index of op with highest precedence
        rti = functools.reduce(lambda x, y: x if l[x][1] > l[y][1] else y, range(len(l)))

        # index of op with lowest precedence
        # rti = -1
        # for li in range(len(l)):
        #     (c, p, _) = l[li]
        #     if c == 'O' and (rti < 0 or l[rti][1] >= p):
        #         rti = li
        # if rti < 0:
        #     if len(l) < 2:
        #         # trivial case, no operator in!
        #         rti = 0 
        #     else:
        #         raise LabelParseException("Uable to detect lowest precedence operator within expression!")

        # take this as root, subdivide eval into left / right sub lists
        root = l[rti]
        OPTIONS.debug(3, ".. root: ", root)

        if root[0] == 'W':
            # evaluate label or numeric value
            # but: not allowing to recurse into the expression, because in this case
            # this(!) parser already would have already detected the stream of symbols!
            # (risk of infinite recursion checkForImmediateExpression <-> eval)
            n = self.__checkForImmediateExpression(root[2], recurse=False, recurseDepth=recurseDepth + 1)
            OPTIONS.debug(3, ".. root W/D eval ", n)
            OPTIONS.indentDec()
            return n

        if root[0] == 'L':
            # degenerated case, single list element
            OPTIONS.debug(3, ".. deeper in singular list element")
            OPTIONS.indentDec()
            return self.evaluateParsedExpression(root[2])

        if root[0] != 'O':
            OPTIONS.indentDec()
            raise LabelParseException("Expression structure mismatch for highest precedence operator!")

        # now, root should be an operator, but could be an binary or unary one ..

        op = root[2]
        unary = rti < 1 and " - ! ~ ¬ ".find(' '+op+' ') >= 0
        if unary:
            OPTIONS.debug(3, ".. unary operator detected!")

        resLeft = None
        if rti > 0 and not unary:
            resLeft = self.evaluateParsedExpression(l[:rti], recurseDepth=recurseDepth+1)
            OPTIONS.debug(3, ".. res left: ", resLeft)
            if resLeft is None:
                OPTIONS.indentDec()
                raise LabelParseException("Expression left sub tree evaluation failed for: %s" % str(l[:rti]))

        resRight = None
        if rti < len(l)-1:
            resRight = self.evaluateParsedExpression(l[rti+1:], recurseDepth=recurseDepth+1)
            OPTIONS.debug(3, ".. res right: ", resRight)
            if resRight is None:
                OPTIONS.indentDec()
                raise LabelParseException("Expression right sub tree evaluation failed for: %s" % str(l[:rti]))

        #
        # now according to OPERATORS
        #
        res = None
        if unary and op == '-':
            res = -resRight
        if unary and op == '!':
            res = 1 if resRight == 0 else 0
        if unary and op == '~':
            res = ~resRight
        if unary and op == '¬':            
            res = Helper.toTwosComplement(resRight, targetBits)
        if op == '+':
            res = resLeft + resRight
        if not unary and op == '-':
            res = resLeft - resRight
        if op == '*':
            res = resLeft * resRight
        if op == '/':
            res = resLeft // resRight
        if op == '%':
            res = resLeft % resRight
        if op == '&':
            res = resLeft & resRight
        if op == '|':
            res = resLeft | resRight
        if op == '^':
            res = resLeft ^ resRight
        if op == '&&':
            f = resLeft != 0 and resRight != 0
            res = 1 if f else 0
        if op == '||':
            f = resLeft != 0 or resRight != 0
            res = 1 if f else 0
        if op == '<<':
            res = resLeft << max(0, resRight)
        if op == '>>':
            res = resLeft >> max(0, resRight)
        if op == '==':
            f = resLeft == resRight
            res = 1 if f else 0
        if op == '!=':
            f = resLeft != resRight
            res = 1 if f else 0
        if op == '<':
            f = resLeft < resRight
            res = 1 if f else 0
        if op == '>':
            f = resLeft > resRight
            res = 1 if f else 0
        if op == '<=':
            f = resLeft <= resRight
            res = 1 if f else 0
        if op == '>=':
            f = resLeft >= resRight
            res = 1 if f else 0

        OPTIONS.debug(3, ".. calculation ", resLeft, " ", op, " ", resRight, " gave ", res)

        if res is None:
            OPTIONS.indentDec()
            raise LabelParseException("Expression operator not properly evaluated!")

        # we have res
        OPTIONS.indentDec()
        return res

    def __checkForImmediateExpression(self, op: str, \
            lowerLimit=-sys.maxsize, upperLimit=sys.maxsize, \
            recurse=True, recurseDepth=0, targetBits=16, orgpos=0):
        """ quick check, if `op` as string will comply to an immediate 
        expression, which can hold lables as well. Will return `None`,
        if false or cannot be evaluated immediately """

        # simple cases
        if op == '$':
            return orgpos

        if op.isdigit():
            n = int(op, 10)
            if n is not None and n >= lowerLimit and n <= upperLimit:
                return n
            else:
                return None

        if op.upper().startswith('$') and all(c in string.hexdigits for c in op[1:]):
            n = int(op[1:], 16)
            if n is not None and n >= lowerLimit and n <= upperLimit:
                return n
            else:
                return None

        if op.upper().endswith('H') and all(c in string.hexdigits for c in op[:-1]):
            n = int(op[:-1], 16)
            if n is not None and n >= lowerLimit and n <= upperLimit:
                return n
            else:
                return None

        if op.upper().endswith("B") and op[:-1].isdigit():
            n = int(op[:-1], 2)
            if n is not None and n >= lowerLimit and n <= upperLimit:
                return n
            else:
                return None

        if len(op) > 1 and ((op.startswith("'") and op.endswith("'")) or \
                (op.startswith('"') and op.endswith('"'))) :
            n = ord(op[1])
            if n >= lowerLimit and n <= upperLimit:
                return n
            else:
                return None

        # check, if expression is actually simply a label
        lb = op.strip().upper()
        if lb in self.labels:
            found = self.labels[lb]
            res = None
            if found.kind == 'L':
                # without big magic, we should be able to take the direct value and give back
                res = found.directValue

            if found.kind == 'E':
                # do we already have a direct value for this label?
                if found.directValue != None:
                    res = found.directValue
                else:
                    # can access an intermediate expression?
                    if found.expression != None:
                        # before going into recursion, check if recurse depths has already been exceeded
                        if recurseDepth > OPTIONS.maxRecurseDepth:
                            raise LabelParseException("Reaching maximum recursion depth while evaluating expression!")

                        res = self.__checkForImmediateExpression(found.expression, lowerLimit=lowerLimit, \
                                upperLimit=upperLimit, targetBits=targetBits, \
                                recurse=True, recurseDepth=recurseDepth + 1) # TODO check recurse!
                        
                        # if there was an result, store it as direct
                        if res != None:
                            found.directValue = res

            # OK, res available?
            if res != None:
                # can take value, have to constrain
                res &= (1 << targetBits) - 1
                OPTIONS.debug(2, ".. eval label %s gave %d!" % (found.tag, res))
                # to be square, also constrain to limits
                if res >= lowerLimit and res <= upperLimit:
                    return res
                else:
                    return None

        # complex case: suspect, that it is a nested expression!
        if recurse:
            l = self.tryParseExpression(op)
            OPTIONS.debugObject(2, "PARSE:", l)
            l = self.checkParsedExpression(l)
            if l is not None:
                OPTIONS.debug(2, ".. check parsed list passed!")
                OPTIONS.indentTo(6)
                res = self.evaluateParsedExpression(l, targetBits=targetBits, recurseDepth=recurseDepth+1)
                if res is not None:
                    OPTIONS.debug(2, ".. eval parsed list gave %d!" % res)
                    # to be square, also constrain to limits
                    if res >= lowerLimit and res <= upperLimit:
                        # TWO'S complement
                        return Helper.toTwosComplement(res, targetBits=8)
                return None

        # no :-(
        return None

    def checkForImmediateExpression(self, op: str, \
            lowerLimit=-sys.maxsize, upperLimit=sys.maxsize, targetBits=16, orgpos=0):
        """ Will check, if `op` as string will comply to an immediate 
        expression, which can hold lables as well. Will return `None`,
        if false or cannot be evaluated immediately """
        
        # this mainly creates one context, in which the raise of LabelParseException
        # will immediately stop recursion
        try: 
            res = self.__checkForImmediateExpression(op, lowerLimit=lowerLimit, upperLimit=upperLimit, \
                    targetBits=targetBits, orgpos=orgpos)
            return res
        except LabelParseException as err:            
            OPTIONS.debug (2, ".. parsing expression %s gave: %s" % (op, str(err)))
            
        return None


#
# ASSEMBLEs
#

class AssembleRecord:
    """ Will track the information of one (partially) assembled line. Will keep the bytes as well.
    Will pass information from one stage to the next .. """

    def __init__(self, orgpos=0, line="", lineno=0, instrParts=None):
        self.orgpos = orgpos                # byte code org pos start
        self.line = line                    # TODO remove later, as waste of space
        self.instrParts = instrParts        # cached decomposed (label, mne, op1, op2, rest)
        self.lineno = lineno
        self.bytes = bytearray()            # idea: efficient storage
        self.invalidBytePos = []            # starting from 0, indicating invalid bytes in array above
        self.score = sys.maxsize
        self.opcodedef = None               # OpCodeDef
        self.oplit = ["",""]                # operands after symbolic substitutions
        self.directive = None               # special directive, if recognized, as string label
        self.opresult = None                # will be a tuple for 2 operand match results
        self.opdata = []                    # list of ints containing (immediate, displacement..) operand data
        self.mtstatelit = []                # Translated ("literally") mtstates from OpCodeDef

    def asDirective(self, directive, mne, op1, label=""):
        """ Initialize important fields for a directive """
        self.directive = directive
        self.instrParts = (label, mne, op1, "", "") 
        self.oplit = (op1, "")

    def abstract(self):
        """ Small to string method. """
        if self.opcodedef is None:
            return "No OpCodeDef"
        else:
            ocd = self.opcodedef
            return "OpCodeDef (%s,%s,%s) with AM %s,%s and LIT %s" % (ocd.opcode, ocd.op1, ocd.op2, ocd.am1, ocd.am2, str(self.oplit))

    def setLabel(self, lb: str):
        """ Change assyrec to have label `lb` set. """
        self.instrParts = (lb,) + self.instrParts[1:]

    def bytesize(self):
        """ return the number of "real" bytes in the record. Used to increase the orgpos after
        each assembly line """
        return len(self.bytes)

    def translateBitPatternsIntoBytes(self):
        """ Will join opcode definitions bit patterns and variable bit patterns
        and will translate this into byte stream (will be reset!).
        Any "open issues" will be marked as invalid byte positions
        Returns `False` in case of any severe error, `True` else. """

        # check, if assyrec is in appropriate state for this step
        if self.opcodedef is None:
            return False

        # reset bytes
        self.bytes = bytearray()
        self.invalidBytePos = []

        # prepare a dict
        bps = {}
        if self.opresult != None:
            for opr in self.opresult:
                if opr is True:
                    continue
                (var, flag, val) = opr
                if flag != 'BP':
                    continue
                bps[var] = val

        # just take each bit pattern
        newbp = []
        for bp in self.opcodedef.bitPattern:
            sum = ""
            errs = 0
            for c in bp:
                if c == '1' or c == '0':
                    sum += c
                    continue
                else:
                    # search for variable key c!
                    if c in bps:
                        if len(bps[c]) < 1:
                            # no more bits left --> error condition
                            sum += '!'
                            errs += 1
                        else:
                            sum += bps[c][0]
                            bps[c] = bps[c][1:]
                    else:
                        # key not found
                        sum += c
                        errs += 1

            # save to be the new bit pattern
            newbp.append(sum)

            # depending on errs, add
            if errs < 1:
                self.bytes.append(int(sum, 2))
            else:
                self.invalidBytePos.append(len(self.bytes))
                self.bytes.append(0)

        # Done
        return True

    def listing(self, digitsPerHexAdress=4, dumpBytesPerLine=4, colPosOperation=80, \
                    shopOperation=True, indicator=' ', compareWithBytes=None, shiftCompare=0):
        """ generates one or more lines of listing """

        # fill the sum with the users input line first
        pos = 0
        cwbchars = 2*dumpBytesPerLine + 3 if compareWithBytes is not None else 0
        dumpchars = cwbchars + digitsPerHexAdress + 2 + 2*dumpBytesPerLine + 1
        sumLine = ' ' * dumpchars + self.line
        sumWrap = ""

        # if the line is longer and operation exists, a wrap needs to occur
        ops = ""
        if self.opcodedef is not None and shopOperation:
            ops = self.opcodedef.operation.strip()

        if len(ops) > 0:
            if len(sumLine) >= cwbchars + colPosOperation - 2:
                # wrap and ops
                sumWrap = sumLine[cwbchars + colPosOperation - 2:]
                sumLine = sumLine[:cwbchars + colPosOperation - 2] + "\u2026|" + ops
            else:
                # fill line and ops
                sumLine = sumLine.ljust(cwbchars + colPosOperation - 2) + " |" + ops        

        # now go into the loop to fill more data
        while True:

            # prepare hex adress
            ha = '{num:{fill}{width}x}'.format(num=self.orgpos + pos, fill='0', width=digitsPerHexAdress) + ':'

            # indicator
            ic = (indicator.ljust(1))[:1]

            # prepare dump bytes
            db = ""
            for i in range(dumpBytesPerLine):
                if pos+i < len(self.bytes):
                    if pos+i in self.invalidBytePos:
                        db += "??"
                    else:
                        db += "{0:02x}".format(self.bytes[pos+i])
                else:
                    db += "  "

            # may be compare with bytes provided as external bytearray
            cwb = ""
            delta = False
            if compareWithBytes is not None:
                for i in range(dumpBytesPerLine):
                    if pos+i < len(self.bytes) and self.orgpos - shiftCompare + pos + i < len(compareWithBytes):
                        cwb += "{0:02x}".format(compareWithBytes[self.orgpos - shiftCompare + pos + i])
                        if self.bytes[pos+i] != compareWithBytes[self.orgpos - shiftCompare + pos + i]:
                            delta = True
                cwb = cwb.ljust(2 * dumpBytesPerLine) + ("<<" if delta else "  ") + ":"

            pos += dumpBytesPerLine

            # implant data into actual sum
            sumLine = cwb + ha + ic + db + "|" + sumLine[dumpchars:]

            # print out
            print(sumLine)

            # when to break?
            if pos >= len(self.bytes) and len(sumWrap) < 1:
                break

            # ok, next line
            extras = max(0, cwbchars + colPosOperation - 1 - (dumpchars + len(sumWrap)))
            sumLine = ' ' * (dumpchars + extras) + sumWrap
            sumWrap = ""

        # done
        return True

    def synthesis(self, digitsPerHexAdress=4, dumpBytesPerLine=4, indicator=' ', tabWidth=8, justOutput=False):
        """ generates one or more lines of synthesized listing """

        # synthesized line
        lab = ""
        if len(self.instrParts[0]) > 0:
            lab = self.instrParts[0].strip()+':'
        line = lab.ljust(tabWidth) + self.instrParts[1] + ' '
        for i in (0,1):
            if len(self.oplit[i]) > 0:
                line += self.oplit[i] + ','
        line = line.rstrip(',')

        # just asm line output
        if justOutput:
            return line

        # fill the sum with the users input line first
        pos = 0
        dumpchars = digitsPerHexAdress + 2 + 2*dumpBytesPerLine + 1
        sumLine = ' ' * dumpchars + line
        sumWrap = ""

        # now go into the loop to fill more data
        while True:

            # prepare hex adress
            ha = '{num:{fill}{width}x}'.format(num=self.orgpos + pos, fill='0', width=digitsPerHexAdress) + ':'

            # indicator
            ic = (indicator.ljust(1))[:1]

            # prepare dump bytes
            db = ""
            for i in range(dumpBytesPerLine):
                if pos+i < len(self.bytes):
                    if pos+i in self.invalidBytePos:
                        db += "??"
                    else:
                        db += "{0:02x}".format(self.bytes[pos+i])
                else:
                    db += "  "

            pos += dumpBytesPerLine

            # implant data into actual sum
            sumLine = ha + ic + db + "|" + sumLine[dumpchars:]

            # print out
            print(sumLine)

            # when to break?
            if pos >= len(self.bytes) and len(sumWrap) < 1:
                break

            # ok, next line
            sumLine = ""

        # done
        return True

    def compareWithBytes(self, compareWithBytes=None, shiftCompare=0):
        """ Expects `compareWithBytes` to be set to alternate binary object.
        Will return a tuple `(bytes, invalids, diffs)` as a statistics. """

        diffs = 0
        invalids = 0
        for i in range(len(self.bytes)):
            if i in self.invalidBytePos:
                invalids += 1
                diffs += 1
            else:
                if self.bytes[i] != compareWithBytes[self.orgpos - shiftCompare + i]:
                    diffs += 1
        
        return (len(self.bytes), invalids, diffs)

class ListOfAssembleRecords(list):
    """ Some convenience functions over a list of assyrecs. """
    def __init__(self, *args):
        list.__init__(self, *args)
        self.orgposToRecord = {}

    def add(self, ar: AssembleRecord):
        """ Adds an `AssembleRecord`, making also the book keeping """
        self.append(ar)
        self.orgposToRecord.setdefault(ar.orgpos, [])
        self.orgposToRecord[ar.orgpos].append(ar)

    def findOrgPos(self, orgpos: int):
        """ Returns a list of all `AssembleRecord`, which have a orgpos. """
        if not orgpos in self.orgposToRecord:
            return []
        return self.orgposToRecord[orgpos]

    def sortedListOfIndexAssyRec(self):
        """ Prepares a list of tuples `(orgpos, assyrec)`, which is sorted
        ascending. """
        l = []
        for ar in self:
            l.append((ar.orgpos, ar))
        # sort
        l.sort(key=lambda k: k[0])
        # done
        return l

    def binarySearchEnclosingAssyRec(self, adr: int, l: list, ll=None, ul=None):
        """ Takes the list from `sortedListOfIndexAssyRec` and detemines
        the `AssembleRecord` enclosing it and returns it. Else `None` """

        # format l: [ .. (orgpos, assyrec) .. ]        
        # start of recursion; easy signature to outside
        if l is None or len(l) < 1:
            return None
        if ll is None or ul is None:
            ll = 0
            ul = len(l)-1
        OPTIONS.debug(3, "binarySearchEnclosingAssyRec: adr=%d ll=%d ul=%d" % (adr, ll, ul))

        # direct hit or miss?
        if ul - ll < 2:
            for i in (ll,ul):
                # looks funny, but works for both cases ll=ul, ll+1=ul
                if l[i][1].orgpos <= adr and l[i][1].orgpos + len(l[i][1].bytes) > adr:
                    return l[i][1]
            return None

        # recursion for binary divided interval
        mid = int((ll + ul) / 2)
        if adr < l[mid][1].orgpos:
            return self.binarySearchEnclosingAssyRec(adr, l, ll, mid)
        else:
            return self.binarySearchEnclosingAssyRec(adr, l, mid, ul)


class Assemble:
    """ Class to assemble a file. Will hold a byte stream """
    def __init__(self, opcodes: OpCodeDefList, syms: SymDict, labels: Labels):
        self.assembly = array.array('B') # TODO check needed
        self.orgpos = 0
        self.opcodes = opcodes
        self.syms = syms
        self.labels = labels
        self.assyrecs = []
        # LATER OPTIMIZATION: use http://code.activestate.com/recipes/59857-filter-a-string-and-only-keep-a-given-set-of-chara/
        # for now: define filter of allowed chars in an operand or expression
        # note: for string constant, there is also a "raw" handling of such strings
        self.allowedOpChars = string.ascii_letters + string.digits + "_$()-+*/" + "'" + '"'

    def clear(self):
        self.assembly = array.array('B')
        self.orgpos = 0
        self.assyrecs = []

    def matchSingleOpcodeDef(self, optemplate, am, opis):
        """ will match the given op1/2 `optemplate` of an opcode definition
        together with the `am` adressing mode spec against an stated
        operand input `opis`.
        Returns: `None` if not match, `True` for a plain success or
                 `( variable, 'BP', bitpattern)` if a match with symbol table could be achieved or
                 `( variable, 'EX', expression)` if a numeric expression were found. """
        # debug
        OPTIONS.debug(3, "matchSingleOpcodeDef: opt:" +  optemplate + " am:" + am + " opis: "+opis)
        # clean operand and AM
        optemplate = optemplate.strip() # need to distinct upper/ lower here!
        am = am.strip().upper()
        # clean even more on user input
        opisraw = opis.strip()
        opis = opis.strip().upper()
        opis = ''.join([c for c in opis if c in self.allowedOpChars])
        # try to detect a symbolic variable / expression portion
        # Note: only ONE {..} is possible :-()
        if optemplate.find("{") >= 0:
            match = re.search(r'({\w+})', optemplate)
            if match is not None:
                key = match.group(1)
                rawkey = key[1:-1]
                (keyStart, keyEnd) = match.span(1)
                #
                # Try apply SYMBOLIC SUBSTITUTION
                #
                if " R M RI B ".find(" "+am+" ") >= 0:
                    # the hard way: find symbol key in symbols and subsequently
                    # try out all symbolic substitutions on opis ..
                    OPTIONS.debug(3, "  .. try out all symbolic substitutions for sym key:" + key)
                    foundSyms = self.syms.get(rawkey)
                    if foundSyms is not None:
                        # we have a list of tuples (key, symbol, bitpattern)
                        OPTIONS.debug(3, "  .. found # of tuples:", len(foundSyms))
                        for (foundKey, foundSymbol, foundBitpattern) in foundSyms:
                            if foundKey != rawkey:
                                # UUPS!
                                OPTIONS.error(104, "Found symbol key mismatch with dictionary!")
                                continue
                            # we will prepare an optemplate, which has the instantiated symbol in it
                            opisLit = optemplate.replace(key, foundSymbol)
                            opisLit = opisLit.upper()
                            # debug
                            OPTIONS.debug(3, "  .. compare literated op:" + opisLit + " against given op:" + opis)
                            # in case of success, opisLit and opis should match char-by-char
                            if opisLit == opis:
                                # form a tuple to give back
                                res = (foundKey[0], 'BP', foundBitpattern)
                                # debug
                                OPTIONS.debugObject(3, "  .. SUCCESS with result:", res)
                                # success!
                                return res
                    # debug
                    OPTIONS.debug(3, "  .. looking for symbolic resolution NOT successful! Aborting!")
                    return None

                #
                # Try apply NUMERIC EXPRESSION
                #
                if " I IE MPZA L E EJ X ".find(" "+am+" ") >= 0:
                    # try cut out numeric expression by crude string manipulation
                    # remark: using opis-raw, which is NOT filtered for allowed chars in order
                    # to support labels and string constants
                    #
                    # Alternative would be to lexically parse the whole operand and match on symbols ..
                    # TODO later: shlex, pyparsing
                    OPTIONS.debug(3, "  .. try cut out numeric expression start %d end %d!" % (keyStart, keyEnd))
                    requiredStart = optemplate[:keyStart]
                    requiredEnd = optemplate[keyEnd:]
                    OPTIONS.debug(3, "  .. user operand needs to start with '%s' and end with '%s'" % (requiredStart, requiredEnd))
                    # do the start/end test
                    if opisraw.startswith(requiredStart) and opisraw.endswith(requiredEnd):
                        # cut out the middle!
                        middleExp = opisraw[len(requiredStart):len(opisraw)-len(requiredEnd)]
                        OPTIONS.debug(3, "  .. user operand FITS and gives %s !!" % (middleExp))

                        # in case of AM beeing Immediate or Immediate Extended, the match
                        # above is a tautology (required start/end = ""). If such, constrain
                        # a bit further
                        if len(requiredStart) + len(requiredEnd) < 1:

                            # for R, I, IE, opisraw MUST NOT be surrounded by brackets!
                            if (am == 'I' or am == 'IE') and (opisraw.startswith("(") \
                                    or opisraw.endswith(")")):
                                # mismatch
                                OPTIONS.debug(3, "  .. AM is I/IE, but user operand is not!")
                                return None

                            # for RI, X,opisraw MUST be surrounded by brackets!
                            if (am == 'RI' or am == 'X') and not (opisraw.startswith("(") \
                                    and opisraw.endswith(")")):
                                # mismatch
                                OPTIONS.debug(3, "  .. AM is RI/X, but user operand is not!")
                                return None

                        # form a tuple to give back
                        res = (rawkey[0], 'EX', middleExp)
                        # debug
                        OPTIONS.debugObject(3, "  .. SUCCESS with result:", res)
                        # success!
                        return res


        #
        # Assume optemplate to have no symbolic variable, but to be fixed
        #
        # debug
        OPTIONS.debug(3, "  .. compare defined op:" + optemplate + " against given op:" + opis)
        # in case of success, optemplate and opis should match char-by-char
        if optemplate.upper() == opis:
            # debug
            OPTIONS.debug(3, "  .. success!")
            # form a tuple to give back
            res = True
            # debug
            OPTIONS.debugObject(3, "  .. result:", res)
            # success!
            return res

    def matchOCDwithOperands(self, ocd: OpCodeDef, mne, op1, op2):
        """ will match an assembler statement (`mne` `op1`, `op2`) against
        the combination of adressing modes ... in opcode definition `ocd`
        Returns `None`, `(True, True)` up to a tuple `(op1result, op2result)` which refers to the
        result tuples of `matchSingleOpcodeDef` """
        #
        # basic check
        #
        if ocd.opcode.strip().upper() != mne.strip().upper():
            return False
        OPTIONS.debug(3, "")
        OPTIONS.debug(3, "MatchOCD [%s,%s,%s AM %s,%s] with user operands %s,%s,%s" % (ocd.opcode, ocd.op1, ocd.op2, ocd.am1, ocd.am2, mne, op1, op2) )

        #
        # Operand (1) has to match, if ocd.op1 is present!
        #
        if len(ocd.op1) == 0:
            # if ocd foresees no op, no op is allowed
            if len(op1.strip()) > 0:
                # mismatch
                return None
            # immediate success (do not expect the case, that op1 is empty and op2 is present!)
            return (True, True)
        # test
        OPTIONS.indentInc()
        OPTIONS.debug(3, "OPERAND (1)")
        OPTIONS.indentInc()
        op1result = self.matchSingleOpcodeDef(ocd.op1, ocd.am1, op1)
        if op1result is None:
            OPTIONS.indentDec()
            OPTIONS.indentDec()
            return None
        OPTIONS.debugObject(3, ".. SUCESSS: op1res", op1result) 

        #
        # Operand (2) has to match, if ocd.op2 is present 
        #
        if len(ocd.op2) == 0:
            # if ocd foresees no op, no op is allowed
            if len(op2.strip()) > 0:
                # mismatch
                return None
            # immediate success for this 2nd stage
            return (op1result, True)
        # test
        OPTIONS.indentDec()
        OPTIONS.debug(3, "OPERAND (2)")
        OPTIONS.indentInc()
        op2result = self.matchSingleOpcodeDef(ocd.op2, ocd.am2, op2)
        if op2result is None:
            OPTIONS.indentDec()
            OPTIONS.indentDec()
            return None
        OPTIONS.debugObject(3, ".. SUCESSS: op2result", op2result) 

        #
        # Succeeded
        #
        OPTIONS.indentDec()
        OPTIONS.indentDec()
        return (op1result, op2result)

    def makeBitPatternString(self, n: int, bplen: int, twoComplement=False):
        """ Turns an integer `n` into a left justified bit pattern string of `len` such as "11001".
        Returns `None` for `n is None` """
        if n is None:
            return None
        # if we have two's complement, we have to binary AND with an bitmask of appropriate
        # len: https://stackoverflow.com/questions/16255496/format-negative-integers-in-twos-complement-representation
        if twoComplement:
            n = Helper.toTwosComplement(n, targetBits=bplen)
            # bm = (1 << bplen) - 1
            # n = -n & bm
        # simply do by hand .. as an exercise ;-)
        res = ""
        for i in range(bplen):
            res = ('1' if (n & 0x01 > 0) else '0') + res
            n = n >> 1
        return res

    def investigateForSpecialDirective(self, assyrec: AssembleRecord, labeltag, mne, op1, op2, rest, stage=1):
        """ Will investigate the given parameters for a possible special
        assembler directive etc.
        Will store appropriate information in `assyrec` (has to exist!), e.g.
        field `directive` to be not `None`.
        Returns `False` in case of any severe error, `True` else. """

        # operand data
        if stage == 1:
            labeltag = labeltag.strip().upper()
            mne = mne.strip().upper()
            op1 = op1.strip()
            op2 = op2.strip()
            rest = rest.strip()
        elif assyrec.instrParts is not None:
            # use cached data
            (labeltag, mne, op1, op2, rest) = assyrec.instrParts
        else:
            OPTIONS.error("Not enough cached data for stage 2+ directive parsing!")
            return False

        if stage == 1 and mne == "ORG":
            # set org pos
            if len(op1) < 1:
                OPTIONS.error(107, "ORG directive needs to have hex address or label as operand!")
                return False

            # try evaluate op1 as expression
            n = self.labels.checkForImmediateExpression(op1, 0, 65535, orgpos=self.orgpos)

            if n is not None:
                OPTIONS.debug(2, ".. setting org pos to $%x" % n)
                self.orgpos = n
                assyrec.directive = "ORG"
            else:
                OPTIONS.error(107, "ORG directive needs to have hex address or label as operand!")
                return False

        if stage == 1 and mne == "EQU":
            # need to have label and operand!
            if len(labeltag) < 1 or len(op1) < 1:
                OPTIONS.error(108, "EQU directive needs to have an identifier and adress/ label as operand!")
                return False

            # because of feared side effects when evaluating the expression below,
            # remove a existing label with this tag (due to initial labelling by line split)
            # before
            self.labels.popIfExist(labeltag)

            # try evaluate op1 as expression
            n = self.labels.checkForImmediateExpression(op1, -sys.maxsize, sys.maxsize, orgpos=self.orgpos)

            # always store as label, even without an immediate value
            newlab = Label(labeltag, 'E', expression=op1)

            if n is not None:
                # we're simply overwriting the already created 'L' label by an 'E' label
                OPTIONS.debug(2, ".. evaluated identifier %s to %d" % (labeltag, n))
                newlab.directValue = n

            assyrec.directive = "EQU"
            self.labels.addOrSet(newlab)

        isDEFB = len(mne) > 0 and " DEFB DB DEFM DM ".find(mne) >= 0
        isDEFW = len(mne) > 0 and " DEFW DW ".find(mne) >= 0

        if isDEFB or isDEFW:
            # define a sequence of data bytes, data words or a message
            # 1st chunk together
            sum = op1
            for x in (op2, rest):
                if len(x) > 0:
                    sum += "," + x
            
            # now split again apart ;-)
            suml = sum.split(sep=",")

            # will result
            assyrec.directive = mne
            assyrec.bytes = bytearray()
            assyrec.invalidBytePos = []

            # over each part
            for part in suml:                
                # exists?
                part = part.strip()
                if len(part) < 1:
                    continue
                
                # is a string?
                if (part.startswith('"') and part.endswith('"')) \
                        or (part.startswith('\'') and part.endswith('\'')):
                    # extract string
                    s = part[1:-1]
                    OPTIONS.debug(2, ".. define bytes by string |%s|" % (s))
                    # unescape string constants
                    try:
                        s = Helper.unescape(s)
                    except ValueError:
                        OPTIONS.error(110, "error unescaping string expression %s %s!" % (mne, s))
                        continue        
                    # sequence of bytes
                    sb = bytearray(s, 'latin_1') # TODO check again
                    # expand to words?
                    if isDEFW:
                        nsb = bytearray()
                        for n in sb:
                            nsb.append(n)
                            nsb.append(0)
                        sb = nsb                       
                    # out
                    OPTIONS.debug(2, ".. got %d bytes in sequence" % (len(sb)))
                    assyrec.bytes += sb
                    continue

                # can be evaluated as an expression?
                if isDEFB:
                    n = self.labels.checkForImmediateExpression(part, 0, 255, targetBits=8, orgpos=self.orgpos)
                    if n is not None:
                        # take as singular value
                        OPTIONS.debug(2, ".. define byte $%x, %d" % (n, n))
                        assyrec.bytes.append(n)
                    else:
                        if stage == 1:
                            OPTIONS.debug(2, ".. %s cannot be evaluated as sequence of bytes!" % (part))
                        else:
                            # more severe
                            OPTIONS.error(109, "%s cannot be evaluated as sequence of bytes!" % (part))
                        # append invalid, to keep the bytes reserved for possible stage 2
                        assyrec.invalidBytePos.append(len(assyrec.bytes))
                        assyrec.bytes.append(0)

                if isDEFW:
                    n = self.labels.checkForImmediateExpression(part, 0, 65535, targetBits=16, orgpos=self.orgpos)
                    if n is not None:
                        # take as singular value
                        OPTIONS.debug(2, ".. define word $%x, %d" % (n, n))
                        assyrec.bytes.append(n & 0xff)
                        assyrec.bytes.append((n & 0xff00) >> 8)
                    else:
                        if stage == 1:
                            OPTIONS.debug(2, ".. %s cannot be evaluated as sequence of bytes!" % (part))
                        else:
                            # more severe
                            OPTIONS.error(109, "%s cannot be evaluated as sequence of bytes!" % (part))
                        # append invalid, to keep the bytes reserved for possible stage 2
                        assyrec.invalidBytePos.append(len(assyrec.bytes))
                        assyrec.bytes.append(0)
                        assyrec.invalidBytePos.append(len(assyrec.bytes))
                        assyrec.bytes.append(0)

        # Done
        return True

    def findBestOpCodeDefinition(self, assyrec: AssembleRecord, mne, op1, op2):
        """ Will find the best possible opcode definition and will store it
        in `assyrec` (has to exist!)
        Returns `False` in case of any severe error, `True` else. """

        # at this point, we should be able to look up the mnemonic
        # in the opcode cluster
        legalOpcodes = self.opcodes.findCluster(mne)

        if legalOpcodes is None:
            OPTIONS.error(103, "cannot find mnemonic " + mne + " in ISA opcode table!")
            assyrec.opcodedef = None
            return False

        # the loop below will cause heavy debug output!
        OPTIONS.debug(3, "")
        OPTIONS.debug(3, "")
        OPTIONS.debug(3, "")
        OPTIONS.debug(2, "findBestOpCodeDefinition mne %s op1 %s op2 %s" % (mne, op1, op2))

        # find a winner by scoring different possibilities
        # (highest score, OpCodeDef, op1result, op2result)
        assyrec.score = sys.maxsize
        assyrec.opcodedef = None
        assyrec.opresult = (None, None)

        # check for each candidate in cluster
        for ocd in legalOpcodes:
            results = self.matchOCDwithOperands(ocd, mne, op1, op2)
            if results is not None:
                # successful match, build a score (less is better)
                # number of instr bytes is very important
                score = 100 * len(ocd.bitPattern)
                # we favour less substitutions over more substituatios
                for i in (0,1):
                    if results[i] is not True:
                        # found a bit pattern tuple
                        score += 10
                # now, take the best
                if score < assyrec.score:
                    assyrec.score = score
                    assyrec.opcodedef = ocd
                    assyrec.opresult = list(results)

        # without syntax error, there shall be a winner!
        if assyrec.score >= sys.maxsize:
            OPTIONS.error(105, "SYNTAX ERROR parsing asm: %s %s %s" % (mne, op1, op2))
            assyrec.opcodedef = None
            return False

        # debug
        OPTIONS.debugObject(2, ".. winning result: ", assyrec)

        # OK
        return True

    def substituteExpressionsWithBitPatterns(self, assyrec: AssembleRecord, stage=1):
        """ Will try to resolve expressions to numerical values, which can be
        turned into bitpatterns directly within `assyrec`.
        If not possible, will inject futures into `assyrec.
        Returns `False` in case of any severe error, `True` else. """

        # check, if assyrec is in appropriate state for this step
        if assyrec is None or assyrec.opcodedef is None:
            return True

        # tupels of assyrec.opresults are:
        # `( variable, 'BP', bitpattern)` if a match with symbol table could be achieved or
        # `( variable, 'EX', expression)` if a numeric expression were found.

        ams = [assyrec.opcodedef.am1, assyrec.opcodedef.am2]
        if assyrec.opresult is None:
            return True
        for i in (0, 1):
            if assyrec.opresult[i] != True:
                (var, flag, val) = assyrec.opresult[i]
                # try turn not-BP elements into BPs .. respecting adressing modes
                bpSubstitute = None
                if flag == 'EX':
                    if ams[i] == 'I' or ams[i] == 'EP':
                        n = self.labels.checkForImmediateExpression(val, 0, 255, orgpos=self.orgpos)
                        bpSubstitute = self.makeBitPatternString(n, 8)
                    if ams[i] == 'IE':
                        n = self.labels.checkForImmediateExpression(val, 0, 65535, orgpos=self.orgpos)
                        bpSubstitute = self.makeBitPatternString(n, 16)
                        # have to reorder: low byte first!
                        if bpSubstitute is not None:
                            bpSubstitute = bpSubstitute[8:16] + bpSubstitute[0:8]
                    if ams[i] == 'MPZA':
                        n = self.labels.checkForImmediateExpression(val, 0, 255, orgpos=self.orgpos)
                        if n is not None and int(n/8) >= 0 and int(n/8) <= 7:
                            bpSubstitute = self.makeBitPatternString(int(n/8), 3)
                    if ams[i] == 'L':
                        # absolute address
                        n = self.labels.checkForImmediateExpression(val, 0, 65535, orgpos=self.orgpos)
                        # now make relative
                        r = None
                        if n is not None:
                            # compute
                            r = n - (self.orgpos+2)
                            # check this
                            if r < -127 or r > 129:
                                OPTIONS.error(119, "Jump target to far away for RELATIVE jump!")    
                                return None
                        bpSubstitute = self.makeBitPatternString(r, 8, twoComplement=True)
                    if ams[i] == 'E' or ams[i] == 'EJ':
                        n = self.labels.checkForImmediateExpression(val, 0, 65535, orgpos=self.orgpos)
                        bpSubstitute = self.makeBitPatternString(n, 16)
                        # have to reorder: low byte first!
                        if bpSubstitute is not None:
                            bpSubstitute = bpSubstitute[8:16] + bpSubstitute[0:8]
                    if ams[i] == 'X':
                        # indexed, just plain displacement
                        n = self.labels.checkForImmediateExpression(val, -128, 127, orgpos=self.orgpos)
                        bpSubstitute = self.makeBitPatternString(n, 8, twoComplement=True)
                
                    # if sucessful, change 'EX' to 'BP'
                    if bpSubstitute is not None:
                        assyrec.opresult[i] = (var, 'BP', bpSubstitute)

                    # for stage 2, we need to indicate errors!
                    if stage == 2 and bpSubstitute is None:
                        OPTIONS.error(114, "Unable to finally evaluate expression %s !" % val)

        # debug
        OPTIONS.debugObject(2, ".. variable bit patterns now: ", assyrec)

        # OK
        return True


    def splitLineIntoLabelInstrComment(self, line=""):
        """ Splits an assymbly line.
        Initially done by an regex, however to many special cases.
        Returns `(label, instruction, comment)` """

        label = ""
        instr = ""
        cmt = ""

        # first seperate off the comment part. The comment starts with the first ';',
        # which is not embraced by single or double quotes

        single = False
        double = False
        ci = -1
        for li in range(len(line)):
            c = line[li]
            if c == '"':
                double = not double 
            if c == "'":
                single = not single
            if c == ';' and not single and not double:
                ci = li
                break
        
        if ci >= 0:
            cmt = line[ci:]
            line = line[:ci]
            
        # for detecting label / instr, use the existing regex

        if (len(line.strip())) < 1:
            # nothing to detect
            return ("", "", cmt)

        match = re.match(r'^((\w+)|)(:|\s+)(.*)$', line)
        if match is None:
            return None

        if match.group(2) is not None:
            label = "" + match.group(2).strip()
        instr = ("" + match.group(4)).strip()

        return (label, instr, cmt)

    def performSymbolicInstructionSubstitution (self, mne, op1, op2):
        """ Checks, if some symbolic substitutions shall be made for the instruction.
        If not returns `None`, if so, returns a new tuple `(mne, op1, op2)` """

        return None
        

    def assembleStage1(self, fn):
        """ Read a asm file and process it as stage 1.
        This means, that all lines of the file are processed and are subsequently converted
        into `assyrecs`. Not all bytes in the byte code can be concluded, as symbols/ labels
        might refer to later definitions.
        `self.assyrecs` are completely kept for a later stage 2.
        Multiple files might be processed. """

        #start
        OPTIONS.debug(1, "Starting assemble stage 1 for: ", fn, "..")
        OPTIONS.setFilename(fn)
        lineno = 0
        self.orgpos = 0

        # start reading file
        with open(fn) as asmfile:
            for line in asmfile:
                
                # make up line 
                line = line.rstrip()
                OPTIONS.debug(2, "Raw input:", line)
                lineno += 1
                OPTIONS.setLineNo(lineno)
                OPTIONS.indentTo(0)

                # MARK?
                if OPTIONS.markAtLineNo is not None:
                    if OPTIONS.markAtLineNo == lineno:
                        print("*MARK*")

                # tab expansion
                if OPTIONS.tabWidth > 0:
                    line = Helper.expandTabs(line, tw=OPTIONS.tabWidth)

                # basic split of line
                basicRes = self.splitLineIntoLabelInstrComment(line)
                if basicRes is None:
                    OPTIONS.error(101, "basic split matching is not working for line: ", line, ". Skipping!")
                    continue

                (label, instr, cmt) = basicRes
                OPTIONS.debug(2, ".. basic split label: ", label, " instr: ", instr, " cmt: ", cmt)

                # do think about labels without instruction!
                if len(label) > 0:
                    # save as normal label
                    self.labels.addOrSet(Label(label, 'L', directValue=self.orgpos))

                # something to work with instruction?
                mne = ""
                op1 = ""
                op2 = ""
                rest = ""

                instr = instr.strip()
                if len(instr) > 0:

                    # try split this again
                    match = re.match(r'^(\w{2,5})(|\s+([^,]*),?([^,]*)(.*))$', instr)
                    if match is None:
                        OPTIONS.error(102, "SYNTAX ERROR parsing instruction: ", instr)
                        continue

                    mne = ("" + match.group(1)).strip()
                    if match.group(3) is not None:
                        op1 = ("" + match.group(3)).strip()
                    if match.group(4) is not None:
                        op2 = ("" + match.group(4)).strip()
                    if match.group(4) is not None:
                        rest = ("" + match.group(5)).strip()

                    # debug again
                    OPTIONS.debug(2, ".. instr split to mne: ", mne, " op1: ", op1, " op2: ", op2, " rest:", rest)

                    # special substitution?
                    specSub = self.performSymbolicInstructionSubstitution(mne, op1, op2)
                    if specSub != None:
                        (mne, op1, op2) = specSub
                        OPTIONS.debug(2, ".. instr substitution to mne: ", mne, " op1: ", op1, " op2: ", op2)

                # make up an AssyRec as well
                assyrec = AssembleRecord(orgpos=self.orgpos, line=line, lineno=lineno, \
                            instrParts=(label, mne, op1, op2, rest))

                # makes only sense WITH an instruction
                if len(instr) > 0:

                    proceed = True

                    # (1) special instruction?
                    if not self.investigateForSpecialDirective(assyrec, label, mne, op1, op2, rest, stage=1):
                        # error messages already done
                        proceed = False

                    # if no directive, then try normal instruction sequence
                    if assyrec.directive is None:

                        # (2) try find an appropriate opcode definition
                        if not self.findBestOpCodeDefinition(assyrec, mne, op1, op2):
                            # error messages already done
                            proceed = False

                    # (3) try read out numerical values for immediate expressions, displacements and
                    # more und turn them to bit patterns
                    if proceed and not self.substituteExpressionsWithBitPatterns(assyrec):
                        # error messages already done
                        proceed = False

                    # (4) try join and translate bit patterns to a byte stream
                    if proceed and not assyrec.translateBitPatternsIntoBytes():
                        # error messages already done
                        proceed = False

                # (5) produce a listing
                if OPTIONS.listStage1:
                    assyrec.listing()

                # (9) not to forget about increasing orgpos properly
                self.orgpos += assyrec.bytesize()

                # do not forget about the assyrec
                self.assyrecs.append(assyrec)

        OPTIONS.debug(1, "Stage 1 completed.")

    def assembleStage2(self):
        """ Iterate the existing `self.assyrecs` and revisit the instructions,
        which feature invalid bytes. Check, that overall byte size remain
        constants. Check, if still invalid bytes exist. """

        # start
        OPTIONS.debug(1, "Starting assemble stage 2 ..")
        noErr = 0
        noInvBytes = 0
        self.orgpos = 0

        # numerically
        for ari in range(len(self.assyrecs)):

            # get access
            assyrec = self.assyrecs[ari]
            bl = len(assyrec.bytes)
            ib = len(assyrec.invalidBytePos)
            OPTIONS.debug(2, "[%d] bl=%d, ib%d: %s" % (assyrec.lineno, bl, ib, assyrec.line))
            OPTIONS.setLineNo(assyrec.lineno)
            OPTIONS.indentTo(0)
            
            # the orgpos is taken from stage 1
            self.orgpos = assyrec.orgpos

            # MARK?
            if OPTIONS.markAtLineNo is not None:
                if OPTIONS.markAtLineNo == assyrec.lineno:
                    print("*MARK*")

            # any real action required
            proceed = True
            if ib >= 0:
            
                # (1') special instruction?
                if not self.investigateForSpecialDirective(assyrec, "", "", "", "", "", stage=2):
                    # error messages already done
                    proceed = False

                # if no directive, then try normal instruction sequence
                if assyrec.directive is None:

                    # (3') try read out numerical values for immediate expressions, displacements and
                    # more und turn them to bit patterns
                    if not self.substituteExpressionsWithBitPatterns(assyrec, stage=2):
                        # error messages already done
                        proceed = False

                # (4') try join and translate bit patterns to a byte stream
                if proceed and not assyrec.translateBitPatternsIntoBytes():
                    # error messages already done
                    proceed = False

            # (5') produce a listing
            if OPTIONS.listStage2:
                assyrec.listing(indicator=('*' if ib > 0 else ' '))

            # stat
            noInvBytes += len(assyrec.invalidBytePos)
            noErr += len(assyrec.invalidBytePos) + (1 if not proceed else 0)

            # some severe error?
            if len(assyrec.bytes) != bl:
                OPTIONS.error(112, "Number of bytes per instruction changed for stage 2! Ignore!")
                continue

            # write back (not sure, if this make sense)
            self.assyrecs[ari] = assyrec

        OPTIONS.debug(1, "Stage 2 completed. %d errors, %d invalid bytes remaining!" % (noErr, noInvBytes))

    def evalBoundariesOfAssyRecs(self):
        """ Evaluates the minmal, maximal address in `self.assyrecs`.
        Returns `(minadr, maxadr)` """

        # evaluate minimal org start
        minadr = sys.maxsize
        maxadr = -1

        for assyrec in self.assyrecs:
            if len(assyrec.bytes) > 0:
                # valid info in this assyrec
                if assyrec.orgpos < minadr:
                    minadr = assyrec.orgpos
                if assyrec.orgpos + len(assyrec.bytes) - 1 > maxadr:
                    maxadr = assyrec.orgpos + len(assyrec.bytes) - 1                

        if minadr == sys.maxsize or maxadr < 0:
            OPTIONS.error(116, "Error evaluating minimal/ maximal org pos. Assuming $0.")
            minadr = 0
            maxadr = 0

        return (minadr, maxadr)

    def compareWithBin(self, fn):
        """ Compare bytes in the `self.assyrecs` with the contents of an external
        binary file. """

        # start
        OPTIONS.debug(1, "Comparing assembled bytes with: ", fn)
        try:
            fh = open(fn, 'rb')
            ba = bytearray(fh.read())
        except Exception as e:
            OPTIONS.error(113, "Error accessing %s gave %s" % (fn, str(e)))

        # outline the binary
        (minadr, maxadr) = self.evalBoundariesOfAssyRecs()
        OPTIONS.debug(1, "Found minimal org pos to be $%x" % minadr)

        # listing with compare
        stats = [0, 0, 0] # bytes, invalids, diffs
        for assyrec in self.assyrecs:

            # MARK?
            if OPTIONS.markAtLineNo is not None:
                if OPTIONS.markAtLineNo == assyrec.lineno:
                    print("*MARK*")

            # list
            assyrec.listing(compareWithBytes=ba, shiftCompare=minadr)

            # compare
            cp = assyrec.compareWithBytes(compareWithBytes=ba, shiftCompare=minadr)
            for i in (0, 1, 2):
                stats[i] += cp[i]

        # done
        OPTIONS.debug(1, "Compare completed. %d bytes compared, %d invalid bytes, %d differences at all." % tuple(stats))

    def outputToBin(self, fn):
        """ Write binary out """

        OPTIONS.debug(1, "Writing binary bytes to: ", fn)

        # outline the binary
        (minadr, maxadr) = self.evalBoundariesOfAssyRecs()
        OPTIONS.debug(1, "Found minimal org pos to be $%x = %d, last byte to be $%x = %d" % (minadr, minadr, maxadr, maxadr))

        # make a blob accordingly
        blob = bytearray()
        blob.extend(b' ' * (1 + maxadr - minadr))

        # zero it!
        for i in range(len(blob)):
            blob[i] = 0x00

        # simply overwrite bytes
        for assyrec in self.assyrecs:

            # MARK?
            if OPTIONS.markAtLineNo is not None:
                if OPTIONS.markAtLineNo == assyrec.lineno:
                    print("*MARK*")

            # copy in
            for i in range(len(assyrec.bytes)):
                blob[assyrec.orgpos - minadr + i] = assyrec.bytes[i]

        # IO
        try:
            fh = open(fn, 'wb')
            fh.write(blob)
            fh.close()
        except Exception as e:
            OPTIONS.error(113, "Error accessing %s gave %s" % (fn, str(e)))

        # done
        OPTIONS.debug(1, "Writing completed. %d bytes written" % len(blob))

#
# RAINBOW Tables
#

class RainbowEntry:
    """ Entry of an rainbow table """
    def __init__(self, ocd):
        self.dummy = 1
        self.ocd = ocd                  # opcode def
        self.symsub = []                # list of (sym, substitution bit pattern)
        self.bitpattern

class RainbowTable:
    """ Rainbow table. Fast, efficient mapping of byte codes to opcode definitions.
    `rank` means byte 0,1,2 in opcode. """
    def __init__(self, rank=0, rootbytes=[]):
        self.rank = rank # either 0,1,2
        self.rootbytes = rootbytes # the bytes in machine code, which are leading to this table
        self.title = ""
        self.entries = {}

    def add(self, key: int, assyrec: AssembleRecord):
        """ Puts an entry into the table. The entry is indexed by a int-key, and shall
        be an `AssembleRecord`. """
        self.entries[key] = assyrec

    def addTable(self, key: int, rb):
        """ Puts an entry into the table. The entry is indexed by a int-key, and shall
        be an `RainbowTable`. """
        self.entries[key] = rb

    def get(self, key: int):
        """ clear """
        if not (key in self.entries):
            return None
        else:
            return self.entries[key]

class SetOfRainbows:
    """ Container for rainbow based instruction decoding. """
    def __init__(self, opcodes: OpCodeDefList, syms: SymDict):
        self.opcodes = opcodes
        self.syms = syms
        self.rainbows = {}

    def getOrCreateRainbow(self, rootbytes):
        """ The set of rainbows maintains one or more rainbow tables. They differ in the way,
        at which position in the bytecode matching process they are relevant. These indicative
        bytes are called here 'rootbytes'.
        There shall be exactly one table with no rootbytes, this is the table where the decoding
        process starts, and exactly one table for each valid sequence of rootbytes.
        This function always points to an valid `RainbowTable` for certain `rootbytes`. If
        necessary, this table is created. """

        # silently fix errors
        if rootbytes is None:
            rootbytes = []
        rootbytes = tuple(rootbytes) # immutable

        # access?
        if rootbytes in self.rainbows:
            return self.rainbows[rootbytes]

        # no, create
        rb = RainbowTable(len(rootbytes), rootbytes)
        self.rainbows[rootbytes] = rb
        return rb

    def get(self, rootbytes=None, title=None):
        """ Look for `rootbytes` or for `title`. Return `None`, else. """

        # title?
        if title is not None:
            for k in self.rainbows.keys():
                if self.rainbows[k].title.strip().lower() == title.strip().lower():
                    return self.rainbows[k]
            return None
        
        # no!

        # silently fix errors
        if rootbytes is None:
            rootbytes = []
        rootbytes = tuple(rootbytes) # immutable

        # access?
        if rootbytes in self.rainbows:
            return self.rainbows[rootbytes]

        # no
        return None

    def prepareSingleOpSubstTuples(self, ops, ams):
        """ If no substitution possible, at least `("","","")` will be in! """

        # any symbolic substitution?
        if " R M RI B MPZA ".find(" "+ams+" ") < 0:
            return [("","","")] # id

        # find curly brackets
        match = re.search(r'({\w+})', ops)
        if match is None:
            return [("","","")] # id

        l = []

        # get the key from the match
        key = match.group(1)
        rawkey = key[1:-1]
        (keyStart, keyEnd) = match.span(1)

        # access corresponding symbol defs
        foundSyms = self.syms.get(rawkey)
        if foundSyms is not None:
            # we have a list of tuples (key, symbol, bitpattern)
            OPTIONS.debug(3, "  .. found # of tuples:", len(foundSyms))
            for (foundKey, foundSymbol, foundBitpattern) in foundSyms:
                if foundKey != rawkey:
                    # UUPS!
                    OPTIONS.error(104, "Found symbol key mismatch with dictionary!")
                    continue
                l.append((foundKey, foundSymbol, foundBitpattern))

        # done
        if len(l) < 1:
            return [("","","")]
        else:
            return l

    def prepare(self):
        """ Starts the preparation. """

        OPTIONS.debug(1, "Preparing rainbow tables ..")

        # we "count" line
        lineno = 0

        # over all known opcodes
        for ocd in self.opcodes.opcodes:

            # start
            lineno +=1
            OPTIONS.debug(2, "%d OpCodeDef (%s,%s,%s) with AM %s,%s" % (lineno, ocd.opcode, ocd.op1, ocd.op2, ocd.am1, ocd.am2))
            OPTIONS.setLineNo(lineno)

            # MARK?
            if OPTIONS.markAtLineNo is not None:
                if OPTIONS.markAtLineNo == lineno:
                    print("*MARK*")

            # we could have up to two symbolic substitution lists
            # lists are containing at least ("","","")
            subst1 = self.prepareSingleOpSubstTuples(ocd.op1, ocd.am1)
            subst2 = self.prepareSingleOpSubstTuples(ocd.op2, ocd.am2)

            # trivial, manual, boring, uninspired cross product ..
            cross = [(x, y) for x in subst1 for y in subst2]
            OPTIONS.debugObject(3,"Cross product = ", cross)

            # now, there should be the cardinality already right
            for cp in cross:

                # prepare the literal operands
                ops = [ocd.op1, ocd.op2]
                for i in (0,1):
                    if len(cp[i][0]) > 0:
                        ops[i] = ops[i].replace("{"+cp[i][0]+"}", cp[i][1])

                # store in common format
                assyrec = AssembleRecord(lineno=lineno, instrParts=("", ocd.opcode, ops[0], ops[1], ""))
                assyrec.opcodedef = ocd
                assyrec.oplit = list(ops) # will work on it

                # store the bitpatterns from the cross product
                # (e1,e2) with e1/2 = (variable, 'BP', bitpattern)
                opresult = [True, True]
                for i in (0,1):
                    if len(cp[i][0]) > 0:
                        opresult[i] = (cp[i][0][0], 'BP', cp[i][2]) # as key, only 1st char!
                assyrec.opresult = tuple(opresult)

                # use the symbolic replacements in cp[i][j] also for replacing the
                # registers in the M/T cycle operations ..

                assyrec.mtstatelit = copy.copy(ocd.mtstate)
                for mti in range(len(assyrec.mtstatelit)):
                    for i in (0,1):
                        if len(cp[i][0]) > 0:
                            assyrec.mtstatelit[mti] = assyrec.mtstatelit[mti].replace("{"+cp[i][0]+"}", cp[i][1])

                # "assemble", as well
                assyres = assyrec.translateBitPatternsIntoBytes()
                OPTIONS.debugObject(3,"Assy record = ", assyrec)

                # now, there should be one or more bytes with fully concluded bitpatterns
                # these are the rootbytes
                rootbytes = []
                for i in range(len(assyrec.bytes)):
                    if i in assyrec.invalidBytePos:
                        # might by absolute/ relative adressing bytes IN BETWEEN :-()
                        # differentiate between I, IE !!
                        if " IE E EJ ".find(" "+ocd.am1+" ") >= 0 or " IE E EJ ".find(" "+ocd.am2+" ") >= 0:
                            # byte part of 16bit
                            rootbytes.append(0x200)
                        else:
                            rootbytes.append(0x100)
                    else:
                        rootbytes.append(assyrec.bytes[i])
                OPTIONS.debugObject(3,".. root bytes + entrykey for it = ", rootbytes)

                # actually, the last valid byte is the entry key, it does not belong to rootbytes
                while len(rootbytes) > 0 and rootbytes[-1] > 0xff:
                    rootbytes.pop()

                if len(rootbytes) < 1:
                    OPTIONS.error(115, "Error getting valid bytecodes for OpCodeDef (%s,%s,%s) with AM %s,%s" % (ocd.opcode, ocd.op1, ocd.op2, ocd.am1, ocd.am2))
                    continue
                else:
                    entrykey = rootbytes.pop()

                # access rainbow and put in entry
                rb = self.getOrCreateRainbow(rootbytes)
                rb.add(entrykey, assyrec)
                OPTIONS.debug(3,".. entry key %d, $%x successfully added." % (entrykey, entrykey))

        #
        # easy to generate rank 1/2 tables
        #

        try:
            for rb in self.rainbows.values():
                # rank 0 -> rank 1
                if len(rb.rootbytes) > 0 and rb.rank == 1:
                    if self.get(()) is not None:
                        self.get(()).addTable(rb.rootbytes[0], rb)
                # rank 1 -> rank 2
                if len(rb.rootbytes) > 1 and rb.rank >= 2:
                    if self.get([rb.rootbytes[0]]) is not None:
                        self.get([rb.rootbytes[0]]).addTable(rb.rootbytes[1], rb)
        except Exception as e:
            OPTIONS.error(118, "Error adding rainbow table links: " + str(e))

        # 
        # MANUAL part ..
        #

        OPTIONS.debug(1, "Adding handcrafted infos to rainbow tables ..")

        try:
            self.get(()).title = "Primary"
            self.get([0xed]).title = "xx80xx"
            self.get([0xcb]).title = "xxBITxx"
            self.get([0xdd]).title = "xxIXxx"
            self.get([0xfd]).title = "xxIYxx"
            self.get([0xdd, 0xcb, 0x100]).title = "xxXBITxx"
            self.get([0xfd, 0xcb, 0x100]).title = "xxYBITxx"
        except Exception as e:
            OPTIONS.error(117, "Error refining rainbow tables: " + str(e))

    def decodeBytes(self, input: list):
        """ Takes up to 4 bytes provided in a list and tries to identify a matching
        instruction.
        Returns an tuple `(# consumed bytes, AssembleRecord)` if found, `(0,None)` else. """

        OPTIONS.debug(2, "decodeBytes input= %s" % (Helper.hexdump(input)))

        rootbytes = []
        ii = 0

        while True:

            OPTIONS.debug(2, "  index %d, rootbytes= %s" % (ii, Helper.hexdump(rootbytes)))

            # take the rainbow belonging to the already found, valid rootbytes
            rb = self.get(rootbytes)
            if rb is None:
                OPTIONS.debug(2, "  NO rainbow found .. quitting!")    
            OPTIONS.debug(2, "  rainbow %s" % rb.title)

            # try get an entry
            if ii >= len(input):
                OPTIONS.debug(2, "  LEN exceeding .. quitting!")
                return (0, None)

            o = rb.get(input[ii])
            if o is None:
                OPTIONS.debug(2, "  NO ENTRY in rainbow .. quitting!")
                return(0, None)

            # already an instruction decoded?
            if isinstance(o, AssembleRecord):
                OPTIONS.debug(2, "  ASSYREC found: %s" % (o.abstract()))

                # make a fresh assyrec, but based on found one
                assyrec = AssembleRecord()
                assyrec.instrParts = copy.copy(o.instrParts)
                assyrec.opcodedef = copy.copy(o.opcodedef)
                assyrec.oplit = copy.copy(o.oplit)
                assyrec.mtstatelit = copy.copy(o.mtstatelit)

                # looking at the addressing modes, conclude if to eat up more bytes
                ams = [assyrec.opcodedef.am1, assyrec.opcodedef.am2]
                databytes = 0
                for am in ams:
                    if am == 'I' or am == 'EP':
                        # immediate or extended port, one byte
                        assyrec.opdata.append(input[ii+1])
                        databytes = 1
                    if am == 'IE' or am == 'E' or am == 'EJ':
                        # immediate extended or extended, two bytes
                        n = (input[ii+2] << 8) + input[ii+1]
                        assyrec.opdata.append(n)
                        databytes = 2
                    if am == 'L':
                        # relative, one byte displacement
                        n = Helper.fromTwosComplement(input[ii+1], targetBits=8)
                        assyrec.opdata.append(n)
                        databytes = 1
                    if am == 'X':
                        # indexed, one byte displacement
                        n = Helper.fromTwosComplement(input[ii+1], targetBits=8)
                        assyrec.opdata.append(n)             
                        databytes = 1

                # advance last instr byte + its data
                ii += 1 + databytes

                return (ii, assyrec)

            # no :-(, may be advance further
            if isinstance(o, RainbowTable):
                rb = o
                OPTIONS.debug(2, "  RAINBOW found: %s" % (rb.title))

                # advance state depending on user data
                rootbytes.append(input[ii])
                ii += 1

                # this SHOULD fit to the rainbow table
                if rb.rank != ii or tuple(rb.rootbytes) != tuple(rootbytes):
                    OPTIONS.debug(2, "  RAINBOW mismatching with state sequence!")
                    return (0, None)

                continue

            # uups, not foreseen!
            OPTIONS.debug(2, "  instruction decoding FAILED!")
            return (0, None)

    def outputRainbowTablesAsHtml(self):
        """ output as HTML table, but to stdout. """

        print("----HTML starts here----")

        kl = list(self.rainbows.keys())
        kl.sort(key=lambda k: (self.rainbows[k].rank, str(self.rainbows[k].rootbytes)))

        for kli in kl:
            rb = self.rainbows[kli]
            print("<!doctype html>")
            print("<html lang=en>")
            print("<style>table, th, td { border: 1px solid black; border-collapse: collapse; }</style>");
            print("<h1>Table: %s, %d, $%s</h1>" % (rb.title, rb.rank, ' '.join('{:02x}'.format(x) for x in rb.rootbytes)))
            print("<table>")
            print("  <tr>")
            print("    <th> / </th>")
            for lownib in range(16):
                print("    <th> $_%01x </th>" % (lownib))
            print("  </tr>")
            for highnib in range(16):
                print("  <tr>")
                print("    <td> $%01x_ </td>" % (highnib))
                for lownib in range(16):
                    key = 16*highnib + lownib
                    cell = ""
                    if key in rb.entries:                        
                        o = rb.entries[key]
                        if isinstance(o, AssembleRecord):
                            # cell = "$%02x=%d " % (key, key)
                            if o.opcodedef is not None:
                                cell += (o.opcodedef.opcode + " " + o.oplit[0] + "," + o.oplit[1]).rstrip(',')
                        if isinstance(o, RainbowTable):
                            cell = ">> %s, %d, $%s" % (o.title, o.rank, ' '.join('{:02x}'.format(x) for x in o.rootbytes))
                    print("    <td>%s</td>" % cell)
                print("  </tr>")
            print("</table>")
            print('<p><a href="http://z80-heaven.wikidot.com/opcode-reference-chart">Compare</a></p>')
            print("</html>")

        print("----HTML stops here!----")

#
# Soft CPU
#

class SoftFunction(Enum):
    NONE = 1
    PURE_INC = 2
    PURE_DEC = 3
    ADD = 4
    ADC = 5
    SUB = 6
    SBC = 7
    INC = 8
    DEC = 9
    AND = 10
    OR = 11
    XOR = 12
    CP = 13
    RL = 14
    RLC = 15
    RR = 16
    RRC = 17
    SLA = 18
    SRA = 19
    SLL = 20
    SRL = 21
    RRD = 22
    ADD_TWO_COMPL_OP2 = 23

class SoftRegister:
    """ Approach: have a iterable register bank with minimal OO features
    per register. """

    def __init__(self, name: str, compositeLow=None, compositeHi=None, latchNum=1, function=SoftFunction.NONE, bits=8):
        self.name = name
        self.latchNum = latchNum
        self.theValue = 0
        if self.latchNum > 1:
            self.theValue = list(map(lambda k: 0, range(latchNum)))
        self.function = function
        self.compositeLow = compositeLow
        self.compositeHi = compositeHi
        self.bits = bits 

    def latch(self, value: int, latchIdx=0, byteIdx=-1):
        if self.latchNum <= 1:
            # mode A: single value, but may be composed
            if byteIdx == 0:
                self.theValue = (self.theValue & 0xff00) | (value & 0x00ff)
            elif byteIdx == 1 and self.bits > 8:
                self.theValue = (self.theValue & 0x00ff) | ((value & 0x00ff) << 8)
                print(self.theValue)
            else:
                self.theValue = value
                if self.compositeLow is not None:
                    self.compositeLow.theValue = value & 0x00ff
                if self.compositeHi is not None:
                    self.compositeHi.theValue = (value & 0xff00) >> 8
        else:
            # mode B: multiple values, but not composed
            if latchIdx < 0 or latchIdx >= self.latchNum:
                return
            self.theValue[latchIdx] = value

    def performFunction(self, function: SoftFunction, value):
        # unary or binary?
        if self.latchNum <= 1:
            # unary
            if function == SoftFunction.NONE:
                return value
            if function == SoftFunction.PURE_INC:
                return value + 1
            if function == SoftFunction.PURE_DEC:
                return value - 1
        else:
            # binary
            if function == SoftFunction.NONE:
                return value[0]
            if function == SoftFunction.ADD:
                return value[0] + value[1]
            if function == SoftFunction.ADD_TWO_COMPL_OP2:
                return value[0] + Helper.fromTwosComplement(value[1])

    def output(self, byteIdx=-1):
        # calculation and HI/LO are mutually exclusive
        if self.function == SoftFunction.NONE:
            # register behaviour and 16bit special case
            v = self.theValue
            if self.compositeLow is not None:
                v = self.compositeLow.theValue
            if self.compositeHi is not None:
                v = v + (self.compositeHi.theValue << 8)
        else:
            # ALU / INCer
            v = self.performFunction(self.function, self.theValue)
        # byte selector?
        if byteIdx == 0:
            return v & 0x00ff
        elif byteIdx == 1:
            return (v & 0xff00) >> 8        
        return v        

    def setFunction(self, function: SoftFunction):
        self.function = function

    @property
    def value(self):
        return self.output()

    @value.setter
    def value(self, newval):
        self.latch(newval)

class SoftCPU:
    """ Software emulated CPU. Pre-stage towards FPGA-CPU, therefore this emulation
    strives to do the things much hardware-alike """

    def addRegister(self, r: SoftRegister):
        self.registers[r.name] = r

    def registerInfo(self):
        """ Short debug string """
        # keys = list(self.registers.keys())
        # keys.sort()
        keys = "PC INSTR A B C D E F H L BC DE HL IX IY ACT TMP I R ALU DISP SP INC2 ABUS DBUS ABUF DBUF".split(sep=' ')
        res = ""
        for k in keys:
            r = self.registers[k]
            if r.value is not None and isinstance(r.value, int):
                res += "" + r.name + " {num:{fill}{width}x} ".format(num=r.value, fill='0', width=int(r.bits/4))
            else:
                res += "" + r.name + " ?? "
        return res
    
    def __init__(self):
        # allocated register bank
        # endianness: https://stackoverflow.com/questions/21639597/z80-register-endianness
        self.registers = {}
        self.addRegister(SoftRegister('A'))
        self.addRegister(SoftRegister('F'))
        self.addRegister(SoftRegister('B'))
        self.addRegister(SoftRegister('C'))
        self.addRegister(SoftRegister('D'))
        self.addRegister(SoftRegister('E'))
        self.addRegister(SoftRegister('H'))
        self.addRegister(SoftRegister('L'))
        self.addRegister(SoftRegister('BC', self.registers['C'], self.registers['B'], bits=16))
        self.addRegister(SoftRegister('DE', self.registers['E'], self.registers['D'], bits=16))
        self.addRegister(SoftRegister('HL', self.registers['L'], self.registers['H'], bits=16))
        self.addRegister(SoftRegister('INSTR'))
        self.addRegister(SoftRegister('I'))
        self.addRegister(SoftRegister('R'))
        self.addRegister(SoftRegister('PC', bits=16))
        self.addRegister(SoftRegister('SP', bits=16))
        self.addRegister(SoftRegister('IX', bits=16))
        self.addRegister(SoftRegister('IY', bits=16))
        self.addRegister(SoftRegister('ACT'))
        self.addRegister(SoftRegister('TMP'))
        self.addRegister(SoftRegister('ALU', latchNum=2, function=SoftFunction.NONE))
        self.addRegister(SoftRegister('DISP', latchNum=2, function=SoftFunction.ADD_TWO_COMPL_OP2, bits=16))
        self.addRegister(SoftRegister('INC2', function=SoftFunction.PURE_INC, bits=16))
        self.addRegister(SoftRegister('ABUF', bits=16))
        self.addRegister(SoftRegister('CBUF'))
        self.addRegister(SoftRegister('DBUF'))
        self.addRegister(SoftRegister('ABUS', bits=16))
        self.addRegister(SoftRegister('DBUS'))
        self.totalCycleCount = 0
        self.memory = bytearray()

    def setMemory(self, orgstart: int, ba: bytearray):
        """ Add a portion of memory to the soft CPU ones """

        if len(self.memory) < orgstart:
            # add a spacing
            self.memory.extend(map(lambda k:0, range(orgstart - len(self.memory))))
            self.memory.extend(ba)
        else:
            for i in range(len(ba)):
                if orgstart+i < len(self.memory):
                    self.memory[orgstart+i] = ba[i]
                else:
                    self.memory.append(ba[i])

    def memoryRead(self, adr: int):
        """ Read byte """
        if adr < 0 or adr >= len(self.memory):
            OPTIONS.debug(1, "Invalid memory read access to $%04x" % adr)
            return 0
        else:
            return self.memory[adr]

    def memoryWrite(self, adr: int, value: int):
        """ Write byte """
        if adr < 0 or adr >= len(self.memory):
            OPTIONS.debug(1, "Invalid memory write access to $%04x" % adr)
        else:
            self.memory[adr] = value & 0x00ff

    def performCycle(self, operations: str):
        """ Performs one HW emulation cycle. `operations` contains a comma divided list of 
        operation labels, such as `DBUF.L.IN`, which would be: "latch data bus buffer inward enabled" """

        self.totalCycleCount += 1
        OPTIONS.debug(2, "performCycle (%d) for operations %s" % (self.totalCycleCount, operations))

        if OPTIONS.markAtLineNo is not None:
            if self.totalCycleCount == OPTIONS.markAtLineNo:
                print("*MARK*")

        # split operations
        if isinstance(operations, str):
            ops = map(lambda k: k.strip().upper(), operations.split(sep=','))
        elif isinstance(operations,list):
            ops = operations
        else:
            ops = []

        # shortcut
        r = self.registers

        # this level of emulation is pretty simple, nearly stupid
        # there is an implicit execution order, which is 
        # basically: address bus, data bus in, data bus out

        for op in ops:

            # address bus

            if op == "BC.OE":
                r['ABUS'].value = r['BC'].value

            elif op == "DE.OE":
                r['ABUS'].value = r['DE'].value

            elif op == "HL.OE":
                r['ABUS'].value = r['HL'].value

            elif op == "PC.OE":
                r['ABUS'].value = r['PC'].value

            elif op == "SP.OE":
                r['ABUS'].value = r['SP'].value

            elif op == "INC2.P":
                r['INC2'].setFunction(SoftFunction.PURE_INC)

            elif op == "INC2.N":
                r['INC2'].setFunction(SoftFunction.PURE_DEC)

            elif op == "INC2.L":
                r['INC2'].value = r['ABUS'].value

            elif op == "INC2.OE":
                r['ABUS'].value = r['INC2'].value

            elif op == "DISP.L.X":
                r['DISP'].latch(r['IX'].value, latchIdx=0)

            elif op == "DISP.L.X":
                r['DISP'].latch(r['IX'].value, latchIdx=0)

            elif op == "DISP.L.Y":
                r['DISP'].latch(r['IY'].value, latchIdx=0)

            elif op == "DISP.L.DBUS":
                r['DISP'].latch(r['DBUS'].value, latchIdx=1)

            elif op == "DISP.OE":
                r['ABUS'].value = r['DISP'].value
            
            elif op == 'ABUF.L':
                OPTIONS.debug(2, ".. set external adress bus: $%x" % r['ABUS'].value)
                r['ABUF'].value = r['ABUS'].value

            # data bus in?
            # TODO: check, if "ignoring" DBUS.value is right approach

            elif op == 'DBUF.L.IN':
                data = self.memoryRead(r['ABUF'].value)
                OPTIONS.debug(2, ".. perform read memory $%04x will be: $%02x" % (r['ABUF'].value, data))
                r['DBUF'].value = data
                r['DBUS'].value = data

            # ALU might also generate data for the dbus

            elif op == "TMP.OE.ALU":
                r['ALU'].latch(r['TMP'].value, latchIdx=1)

            elif op == "ACT.OE":
                r['ALU'].latch(r['ACT'].value, latchIdx=0)

            elif op == "ALU.OP.ADD":
                r['ALU'].setFunction(SoftFunction.ADD)

            elif op == "ALU.OE":
                r['DBUS'].value = r['ALU'].value

            # register file

            elif op == "INSTR.L":
                r['INSTR'].value = r['DBUS'].value

            elif op == "ACT.L.DBUS":
                r['ACT'].value = r['DBUS'].value

            elif op == "ACT.L.A":
                r['ACT'].value = r['A'].value

            elif op == "TMP.L":
                r['TMP'].value = r['DBUS'].value

            elif op == "A.L":
                r['A'].value = r['DBUS'].value

            elif op == "B.L" or op == "BC.H.L":
                r['B'].value = r['DBUS'].value

            elif op == "C.L" or op == "BC.L.L":
                r['C'].value = r['DBUS'].value

            elif op == "D.L" or op == "DE.H.L":
                r['D'].value = r['DBUS'].value

            elif op == "E.L" or op == "DE.L.L":
                r['E'].value = r['DBUS'].value

            elif op == "H.L" or op == "HL.H.L":
                r['H'].value = r['DBUS'].value

            elif op == "L.L" or op == "HL.L.L":
                r['L'].value = r['DBUS'].value

            elif op == "IX.L.L":
                r['IX'].latch(r['DBUS'].value, byteIdx=0)

            elif op == "IX.H.L":
                r['IX'].latch(r['DBUS'].value, byteIdx=1)

            elif op == "IY.L.L":
                r['IY'].latch(r['DBUS'].value, byteIdx=0)

            elif op == "IY.H.L":
                r['IY'].latch(r['DBUS'].value, byteIdx=1)

            elif op == "SP.L.L":
                r['SP'].latch(r['DBUS'].value, byteIdx=0)

            elif op == "SP.H.L":
                r['SP'].latch(r['DBUS'].value, byteIdx=1)

            elif op == "PC.L.DBUS":
                r['PC'].value = r['INC2'].value

            elif op == "PC.L.INC2":
                r['PC'].value = r['INC2'].value

            elif op == "SP.L.INC2":
                r['SP'].value = r['INC2'].value

            elif op == "I.L":
                r['I'].value = r['DBUS'].value

            elif op == "R.L":
                r['R'].value = r['DBUS'].value

            # data bus -> address bus .. later that the ...IOError

            elif op == "ABUS.L.L.DBUS":
                r['ABUS'].latch(r['DBUS'].value, byteIdx=0)

            elif op == "ABUS.H.L.DBUS":
                r['ABUS'].latch(r['DBUS'].value, byteIdx=1)

            # data bus out

            elif op == "TMP.OE.DBUS":
                r['DBUS'].value = r['TMP'].value

            elif op == "A.OE" or op == "A.OE.DBUS" or op == "AF.H.OE.DBUS":
                r['DBUS'].value = r['A'].value

            elif op == "A.OE.ACT":
                r['ACT'].value = r['A'].value

            elif op == "F.OE" or op == "AF.L.OE":
                r['DBUS'].value = r['F'].value

            elif op == "B.OE" or op == "BC.H.OE":
                r['DBUS'].value = r['B'].value

            elif op == "C.OE" or op == "BC.L.OE":
                r['DBUS'].value = r['C'].value

            elif op == "D.OE" or op == "DE.H.OE":
                r['DBUS'].value = r['D'].value

            elif op == "E.OE" or op == "DE.L.OE":
                r['DBUS'].value = r['E'].value

            elif op == "H.OE" or op == "HL.H.OE":
                r['DBUS'].value = r['H'].value

            elif op == "L.OE" or op == "HL.L.OE":
                r['DBUS'].value = r['L'].value

            elif op == "SP.H.OE":
                r['DBUS'].value = r['SP'].output(byteIdx=1)

            elif op == "SP.L.OE":
                r['DBUS'].value = r['SP'].output(byteIdx=0)

            elif op == "IX.H.OE":
                r['DBUS'].value = r['IX'].output(byteIdx=1)

            elif op == "IX.L.OE":
                r['DBUS'].value = r['IX'].output(byteIdx=0)

            elif op == "IY.H.OE":
                r['DBUS'].value = r['IY'].output(byteIdx=1)

            elif op == "IY.L.OE":
                r['DBUS'].value = r['IY'].output(byteIdx=0)

            elif op == "I.OE":
                r['DBUS'].value = r['I'].value

            elif op == "R.OE":
                r['DBUS'].value = r['R'].value

            # memory out

            elif op == 'DBUF.L.OUT':
                data = r['DBUS'].value
                OPTIONS.debug(2, ".. perform write memory $%04x will be: $%02x" % (r['ABUF'].value, data))
                r['DBUF'].value = data
                self.memoryWrite(r['ABUF'].value, data)

            # not found?!
            else:
                OPTIONS.error(123, "MT operation %s not found at PC $%04x" % (op, r['PC'].value))

        # Done
        OPTIONS.debug(2, "End of cycle. Registers: %s" % self.registerInfo())

#
# DISASSEMBLE
#

class DisAssemble:
    """ Class to disassemble a file.  """
    def __init__(self, opcodes: OpCodeDefList, syms: SymDict, labels: Labels, sr: SetOfRainbows):
        self.opcodes = opcodes
        self.syms = syms
        self.labels = labels
        self.sr = sr
        self.assyrecs = ListOfAssembleRecords()

    def bytesToStringCommaSep(self, bytes):
        """ Trivial """
        line = ""
        for db in bytes:
            line+= "%02xh," % db
        return line.rstrip(',')

    def addDefBytes(self, orgpos: int, bytes, bytesPerLine=16):
        """ Add one or more assyrecs for DEFB """
        ba = bytearray()
        num = 0
        for db in bytes:

            # new record?
            if num >= bytesPerLine:
                num = 0
                line = self.bytesToStringCommaSep(ba)
                ar = AssembleRecord(orgpos=orgpos-len(ba), line="DEFB "+line)
                ar.asDirective('DEFB', "DEFB", line)
                ar.directive = 'DEFB'
                ar.oplit = (line, "")
                ar.bytes = ba
                self.assyrecs.add(ar)
                ba = bytearray()

            # add
            ba.append(db)
            num += 1
            orgpos += 1

        # rest
        if len(ba) > 0:
            # again
            line = self.bytesToStringCommaSep(ba)
            ar = AssembleRecord(orgpos=orgpos-len(ba), line="DEFB "+line)
            ar.asDirective('DEFB', "DEFB", line)
            ar.directive = 'DEFB'
            ar.bytes = ba
            self.assyrecs.add(ar)

    def assignLabelsToAssyRecs(self, createNewOrgs=False):
        """ Take the defined labels and assign them to appropriate assy recs.
        Split them, if necessars. """

        # place labels
        idx = self.assyrecs.sortedListOfIndexAssyRec()
        for lb in self.labels.labels.values():

            # ideally, the labels have a directValue and are optimally aligned 
            # with assy records.
            if lb.directValue is None or lb.kind != 'L':
                continue
            OPTIONS.debug(2,"Place label %s at $%x" % (lb.tag, lb.directValue))            

            # find an assy rec enclosing it
            ar = self.assyrecs.binarySearchEnclosingAssyRec(lb.directValue, idx)
            if ar is None:
                if not createNewOrgs:
                    OPTIONS.error(121, "Error placing label %s @ %04x .. No matching instruction!" % (lb.tag, lb.directValue)) 
                    continue

                # ok, create new assy recs containing orgs
                OPTIONS.debug(2, "Creating new label/ data for %s @ %04x .." % (lb.tag, lb.directValue)) 

                # we need a pair: an org, and an byte/NOP
                ar2 = AssembleRecord(orgpos=lb.directValue)
                ar2.asDirective('ORG', "ORG", "%xh" % lb.directValue)
                self.assyrecs.add(ar2)

                if lb.tag.upper().startswith("DA"):
                    # data                    
                    ar3 = AssembleRecord(orgpos=lb.directValue)
                    ar3.asDirective('DFB', "DEFB", "0", label=lb.tag)
                    self.assyrecs.add(ar3)
                else:
                    # command
                    ar3 = AssembleRecord(orgpos=lb.directValue, instrParts=(lb.tag, "NOP", "", "", ""))
                    self.assyrecs.add(ar3)

                # trusting, that a final sort with set the assy recs into right order
                continue                    
                    

            if ar.orgpos == lb.directValue:

                # assyrec matches perfectly, simply set/ add label
                OPTIONS.debug(2, ".. found label to match assyrec orgpos!")

                # could happen, that assyrec already has a label!
                if len(ar.instrParts[0]) > 0:
                    # make a 2nd assy rec with only the label at same orgpos
                    OPTIONS.debug(2, ".. adding 2nd label %s to already existing label %s!" % (lb.tag, ar.instrParts[0])) 
                    ar2 = AssembleRecord(ar.orgpos, instrParts=(lb.tag, "", "", "", ""))
                    self.assyrecs.add(ar2)
                else:
                    # modify ar
                    OPTIONS.debug(2, ".. setting label.")
                    ar.setLabel(lb.tag)

            else:

                # assyrec encloses label address, are we able to split the assyrec
                # only if we have data bytes!
                if ar.directive == 'DEFB':

                    # split bytes
                    bytes2 = ar.bytes[lb.directValue - ar.orgpos:]
                    ar.bytes = ar.bytes[0:lb.directValue - ar.orgpos]

                    # make shorter lines
                    line2 = self.bytesToStringCommaSep(bytes2)
                    line = self.bytesToStringCommaSep(ar.bytes)

                    # make a 2nd assy rec being also an DEFB
                    ar2 = AssembleRecord(orgpos=lb.directValue, line="DEFB " + line2)
                    ar2.asDirective('DEFB', "DEFB", line2, label=lb.tag)
                    ar2.directive = 'DEFB'
                    ar2.bytes = bytes2

                    # modify the old ar
                    ar.line = "DEFB" + line
                    ar.oplit = (line, "")

                    # now add a re-compute for binary search
                    self.assyrecs.add(ar2)
                    idx = self.assyrecs.sortedListOfIndexAssyRec()
                else:
                    OPTIONS.error(120, "Error assigning label %s @ %04x. No matching to start of any instruction!" % (lb.tag, lb.directValue))
                    

    def disassemble(self, fn: str, orgstart: int, orgend: int, datasections=None, reuseLabels=True):
        """ Disassemble """

        # read into byte array
        OPTIONS.debug(1, "Disassembling binary file: ", fn)
        try:
            fh = open(fn, 'rb')
            ba = bytearray(fh.read())
        except Exception as e:
            OPTIONS.error(113, "Error accessing %s gave %s" % (fn, str(e)))

        pos = 0
        potdata = []

        # pseudo directive at beginning
        assyrec = AssembleRecord(orgpos=orgstart)
        assyrec.asDirective('ORG', "ORG", "%02xh" % orgstart)
        self.assyrecs.add(assyrec)

        # simply loop        
        while pos < len(ba):

            # did we pass org-end?
            if orgend >= 0 and (pos+orgstart) >= orgend:
                OPTIONS.debug(2, "ORG-END detected. Concluding with def bytes")
                self.addDefBytes(orgstart + pos, ba[pos:], bytesPerLine=16)
                break

            # did we pass a data section?
            if datasections is not None:
                newLoop = False
                for ds in datasections:
                    if (pos+orgstart) >= ds[0] and (pos+orgstart) <= ds[1]:
                        # make a skip over the data section
                        self.addDefBytes(orgstart + pos, ba[pos:ds[1]-orgstart+1], bytesPerLine=16)
                        # reset position
                        pos = ds[1]-orgstart + 1
                        # continue decoding without parsing instruction!
                        newLoop = True
                if newLoop:
                    continue

            # the largest instruction is about 4 bytes
            buf = []
            for i in range(4):
                if pos+i < len(ba):
                    buf.append(ba[pos+i])

            # test decode
            OPTIONS.debug(2, "Test decode pos %d" % pos)
            (nobytes, assyrec) = self.sr.decodeBytes(buf)

            # success?
            if nobytes < 1:
                OPTIONS.debug(2, ".. no success .. potential data? .. advance ..")
                potdata.append(ba[pos])
                pos += 1 
                continue

            # ok? 
            # TODO: deal with potential data

            # decode instruction
            OPTIONS.debugObject(2, "AssyRec = ", assyrec)

            # put the raw bytes in
            assyrec.orgpos = orgstart + pos
            assyrec.bytes = bytearray(buf[0:nobytes])

            # jump target?
            ams = [assyrec.opcodedef.am1, assyrec.opcodedef.am2]
            for i in (0,1):

                if (ams[i] == 'I' or ams[i] == 'IE') and len(assyrec.opdata) > 0:
                    # immediate
                    n = assyrec.opdata[0]
                    # put accordingly to the operand
                    assyrec.oplit[i] = "%xh" % n
                    # fully decoded
                    assyrec.invalidBytePos = []

                if ams[i] == 'E' and len(assyrec.opdata) > 0:
                    # extended -> pointer to normal data
                    adr = assyrec.opdata[0]
                    # assume that (xx) is a label
                    tag = self.labels.reuseLabelForDirectValueOrGenerateNew(adr, prefix="DA", reuseLabels=reuseLabels)
                    # put accordingly to the operand
                    # assyrec.oplit[i] = "(%xh)" % adr
                    assyrec.oplit[i] = "(%s)" % tag
                    # fully decoded
                    assyrec.invalidBytePos = []

                if ams[i] == 'EP' and len(assyrec.opdata) > 0:
                    # extended port -> pointer to one byte I/O port
                    port = assyrec.opdata[0]
                    # put accordingly to the operand
                    assyrec.oplit[i] = "(%xh)" % port
                    # fully decoded
                    assyrec.invalidBytePos = []

                if ams[i] == 'X' and len(assyrec.opdata) > 0:
                    # extended -> IX/IY + displacement
                    disp = assyrec.opdata[0]
                    # not very elegantly
                    isY = (assyrec.opcodedef.op1, assyrec.opcodedef.op2)[i].upper().find("Y") >= 0
                    # put accordingly to the operand
                    assyrec.oplit[i] = "(I%s+%02xh)" % ( 'Y' if isY else 'X', disp)
                    # fully decoded
                    assyrec.invalidBytePos = []

                if ams[i] == 'EJ' and len(assyrec.opdata) > 0:
                    # absolute jump
                    dest = assyrec.opdata[0]
                    # make a label
                    # TODO: can we reuse a label?
                    tag = self.labels.reuseLabelForDirectValueOrGenerateNew(dest, prefix="JP", reuseLabels=reuseLabels)
                    # put the label in place of the operand
                    assyrec.oplit[i] = tag
                    # fully decoded
                    assyrec.invalidBytePos = []
                    
                if ams[i] == 'L' and len(assyrec.opdata) > 0:
                    # relative jump
                    dest = orgstart + pos + 2 + assyrec.opdata[0]
                    # make a label
                    # TODO: can we reuse a label?
                    tag = self.labels.reuseLabelForDirectValueOrGenerateNew(dest, prefix="JR", reuseLabels=reuseLabels)
                    # put the label in place of the operand
                    assyrec.oplit[i] = tag
                    # fully decoded
                    assyrec.invalidBytePos = []

            # advance
            self.assyrecs.add(assyrec)
            pos += nobytes

        # Assign labels
        if True:
            self.assignLabelsToAssyRecs(createNewOrgs=True)

        # sort assy recs ascending
        self.assyrecs.sort(key=lambda ar: (ar.orgpos, ar.instrParts[0].strip() ))

        # Done
        OPTIONS.debug(1, "Disassembling completed.")

    def softExecuteBinary(self, fn: str, orgstart: int, orgend: int, cpu: SoftCPU=None):
        """ Soft execute binary. Decode every single instruction (like dis-asm) and 
        call soft CPU for the respective bytes. """

        # read into byte array
        OPTIONS.debug(1, "Soft-executing binary file: ", fn)
        try:
            fh = open(fn, 'rb')
            ba = bytearray(fh.read())
        except Exception as e:
            OPTIONS.error(113, "Error accessing %s gave %s" % (fn, str(e)))

        pos = 0
        potdata = []

        # set the memory for the soft cpu
        if cpu is not None:
            cpu.setMemory(orgstart, ba)
            cpu.registers['PC'].value = orgstart

        # simply loop        
        while pos < len(ba):

            # did we pass org-end?
            if orgend >= 0 and (pos+orgstart) >= orgend:
                OPTIONS.debug(2, "ORG-END detected. Quitting.")
                break

            # the largest instruction is about 4 bytes
            buf = []
            for i in range(4):
                if pos+i < len(ba):
                    buf.append(ba[pos+i])

            # test decode
            OPTIONS.debug(2, "Test decode pos %d" % pos)
            (nobytes, assyrec) = self.sr.decodeBytes(buf)

            # success?
            if nobytes < 1:
                OPTIONS.error(122, "Error decoding instruction at %x .. Aborting!")
                break

            # decode instruction
            OPTIONS.debugObject(2, "AssyRec = ", assyrec)

            # put the raw bytes in
            assyrec.orgpos = orgstart + pos
            assyrec.bytes = bytearray(buf[0:nobytes])

            # try to soft-cpu the bytes
            if cpu is not None:
                # enforce correct PC
                cpu.registers['PC'].value = orgstart + pos

                # play cycles on the CPU
                for i in range(len(assyrec.mtstatelit)):
                    mts = assyrec.mtstatelit[i].strip()
                    OPTIONS.debug(2, "MT state (%d) is %s" % (i, mts))
                    if mts == "X":
                        OPTIONS.debug(2, ".. pause one cycle!")
                        continue
                    if mts == "-" or mts == "":
                        OPTIONS.debug(2, ".. empty")
                        continue
                    cpu.performCycle(assyrec.mtstatelit[i])

            # advance
            self.assyrecs.add(assyrec)
            pos += nobytes

        # Done
        OPTIONS.debug(1, "Disassembling completed.")

    def listSynthesis(self, listLabels=True):
        """ Listing synthesized information """

        OPTIONS.debug(1, "Starting synthesized assembly ..");

        # listing with compare
        stats = [0, 0, 0] # bytes, invalids, diffs
        for assyrec in self.assyrecs:

            # list
            assyrec.synthesis()

            # stats
            stats[0] += len(assyrec.bytes)
            stats[1] += len(assyrec.invalidBytePos)

        OPTIONS.debug(1, "Listing synthesized assembly completed. %d bytes, %d invalid!" % (stats[0], stats[1])) 

        if listLabels:
            print()
            print("; +++ global symbols +++")
            print()
            self.labels.listing()

    def outputDisAssembly(self, fn):
        """ Writes the assembly out """

        OPTIONS.debug(1, "Writing dis-assembly to file %s .." % fn)

        # IO
        try:
            fh = open(fn, 'w')

            for assyrec in self.assyrecs:

                out = assyrec.synthesis(justOutput=True)
                fh.write(out +  "\n")

            fh.close()
        except Exception as e:
            OPTIONS.error(113, "Error accessing %s gave %s" % (fn, str(e)))        

        OPTIONS.debug(1, "Writing %d records completed." % len(self.assyrecs))


#
# MAIN
#

def dizzy():
    """ Main function """
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", help="increase verbosity", action="count")
    parser.add_argument("-s1", "--list-stage-1", help="list stage 1", action="store_true")
    parser.add_argument("-s2", "--list-stage-2", help="list stage 2", action="store_true")
    parser.add_argument("-ll", "--list-labels", help="list labels/ symbols", action="store_true")
    parser.add_argument("-se", "--skip-errors", help="skip encountered errors", action="store_true")
    parser.add_argument("-tw", "--tab-width", help="tab width for expanding lines", action="store")
    parser.add_argument("-cb", "--compare-bin", help="compare assembly listing with external binary file", action="store")
    parser.add_argument("-ml", "--mark-at-line", help="send MARK to console when reaching line number or total cycle count", action="store")
    parser.add_argument("-t", "--tables", help="specify TWO! Z80 ISA tables as following", nargs='*', action="store")
    parser.add_argument("-a", "--assemble", help="assemble the source file following", action="store")
    parser.add_argument("-o", "--out-file", help="output file to following file name", action="store")
    parser.add_argument("-d", "--disassemble", help="disassemble the binary file following", action="store")
    parser.add_argument("-lt", "--list-tables", help="list generated tables in debug output", action="store_true")
    parser.add_argument("-ls", "--list-synthesis", help="list synthesized disassembly", action="store_true")
    parser.add_argument("-os", "--org-start", help="assume start address of binary", action="store")
    parser.add_argument("-oe", "--org-end", help="proposes address, where code ends and data only starts", action="store")
    parser.add_argument("-ds", "--data-section", help="(multiple) pair of start,end address", action="append")
    parser.add_argument("-e", "--soft-execute", help="read binary file and feed to soft CPU", action="store")
    args = parser.parse_args()

    # verbosity
    if args.verbose is not None:
        OPTIONS.verbose = args.verbose
    OPTIONS.listStage1 = args.list_stage_1
    OPTIONS.listStage2 = args.list_stage_2
    OPTIONS.skipErrors = args.skip_errors
    if args.tab_width is not None:
        OPTIONS.tabWidth = args.tab_width
    if args.mark_at_line and args.mark_at_line.isdigit():
        OPTIONS.markAtLineNo = int(args.mark_at_line)
    OPTIONS.debug(1, "DiZZy v0.1, dead slow Z80 assembler tools, (c) 2017 Michael Hoffmeister")

    # clean tables
    opcodes = OpCodeDefList()
    syms = SymDict()

    # handle tables fn and read
    fn_isa = "z80_isa.csv"
    fn_syms = "z80_syms.csv"
    if args.tables is not None:
        if len(args.tables) > 0:
            fn_isa = args.tables[0]
        if len(args.tables[0]) > 1:
            fn_syms = args.tables[1]
    OPTIONS.debug(1, "Loading tables " + fn_isa +", " + fn_syms + "..")
    opcodes.readTSV(fn_isa)
    syms.readTSV(fn_syms)

    # make clean labels
    labels = Labels()

    # assemble?
    if args.assemble is not None:

        # asm
        OPTIONS.debug(1, "Assembling " + args.assemble + "..")
        a = Assemble(opcodes, syms, labels)
        a.assembleStage1(args.assemble)
        a.assembleStage2()

        # label listing
        if args.list_labels:
            print()
            print("; +++ global symbols +++")
            print()
            labels.listing()

        # compare
        if args.compare_bin is not None:
            a.compareWithBin(args.compare_bin)

        # output
        if args.out_file is not None:
            a.outputToBin(args.out_file)

    # disassemble?
    if args.disassemble is not None:

        # disasm
        OPTIONS.debug(1, "Disassembling " + args.disassemble + "..")

        # tables
        sr = SetOfRainbows(opcodes, syms)
        sr.prepare()

        # debug?
        if args.list_tables:
            sr.outputRainbowTablesAsHtml()

        # tests
        if False:
            sr.decodeBytes([0x78])
            sr.decodeBytes([0x23])
            sr.decodeBytes([0xcb, 0x16])
            sr.decodeBytes([0x3e, 0x5d])
            sr.decodeBytes([0x21, 0x23, 0x12])
            sr.decodeBytes([0x01, 0x1e, 0x00])
            sr.decodeBytes([0x18, 0xef])
            sr.decodeBytes([0xc3, 0x00, 0x00])

        # org?
        orgstart = 0
        if args.org_start is not None:
            n = labels.checkForImmediateExpression(args.org_start, 0, 65535, targetBits=16, orgpos=0)
            if n is not None:
                orgstart = n
        orgend = -1
        if args.org_end is not None:
            n = labels.checkForImmediateExpression(args.org_end, 0, 65535, targetBits=16, orgpos=0)
            if n is not None:
                orgend = n
        OPTIONS.debug(1,"Assuming org-start $%04x, org-end $%04x .." % (orgstart, orgend))

        # data sections
        datasections = []
        if args.data_section is not None:
            for ds in args.data_section:
                match = re.search(r'(\w+)(,|-|;)(\w+)', ds)
                if match is not None:
                    n1 = labels.checkForImmediateExpression(match.group(1), 0, 65535, targetBits=16, orgpos=0)
                    n2 = labels.checkForImmediateExpression(match.group(3), 0, 65535, targetBits=16, orgpos=0)
                    if n1 is not None and n2 is not None:
                        if n2 < n1:
                            n1, n2 = n2, n1
                        datasections.append((n1,n2))
                        OPTIONS.debug(1,"Declaring data section $%04x - $%04x .." % (n1, n2))

        # try
        d = DisAssemble(opcodes, syms, labels, sr)
        d.disassemble(args.disassemble, orgstart, orgend, datasections, reuseLabels=True)

        # listing?
        if args.list_synthesis:
            d.listSynthesis(listLabels = args.list_labels)

        # output
        if args.out_file is not None:
            d.outputDisAssembly(args.out_file)

    # soft execute
    if args.soft_execute is not None:

        # disasm
        OPTIONS.debug(1, "Soft executing " + args.soft_execute + "..")

        # tables
        sr = SetOfRainbows(opcodes, syms)
        sr.prepare()

        # debug?
        if args.list_tables:
            sr.outputRainbowTablesAsHtml()

        # org?
        orgstart = 0
        if args.org_start is not None:
            n = labels.checkForImmediateExpression(args.org_start, 0, 65535, targetBits=16, orgpos=0)
            if n is not None:
                orgstart = n
        orgend = -1
        if args.org_end is not None:
            n = labels.checkForImmediateExpression(args.org_end, 0, 65535, targetBits=16, orgpos=0)
            if n is not None:
                orgend = n
        OPTIONS.debug(1,"Assuming org-start $%04x, org-end $%04x .." % (orgstart, orgend))

        # try
        d = DisAssemble(opcodes, syms, labels, sr)
        cpu = SoftCPU()
        # cpu.registers['PC'].value = 0x100
        # cpu.registers['B'].value = 0x42
        # cpu.performCycle("PC.OE, ABUF.L, INC2.P")
        # cpu.performCycle("INC2.P, INC2.L, INC2.OE, DBUF.IN")
        # cpu.performCycle("INC2.P, INC2.OE, PC.L.INC2, DBUF.IN, INSTR.L")
        # cpu.performCycle("B.OE, TMP.L")
        # cpu.performCycle("TMP.OE.DBUS, C.L")
        d.softExecuteBinary(args.soft_execute, orgstart, orgend, cpu)


if __name__ == "__main__":

    dizzy()
