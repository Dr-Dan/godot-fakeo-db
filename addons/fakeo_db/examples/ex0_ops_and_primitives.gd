tool
extends EditorScript

# ==============================================================	

"""
To use: File > Run
(Ctrl+Shift+X)

An example showing Fakeo with lists of primitves. i.e. not Objects.
"""

const ex_util = preload("res://addons/fakeo_db/examples/example_utils.gd")
const Fko = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const Ops = Fko.OpFactory

func _run():
	ex_util.pr_break('#')
	print('Nested arrays of primitives')
	ex_util.pr_break()
	nested_arrays()
	
	ex_util.pr_break('#')
	print("Using a collection of primitives (int)\n")
	ex_util.pr_break()
	operators()

# ==============================================================	

func nested_arrays():
	var chain = Fko.chain()

	var nested = [[0, [1, 2], 3], [4, [5, 6]], [7,[8],[9, 10]]]
	printt('data:', nested, '\n')
	
	var open_idx = [
		{
			msg='x[2] for each',
			op=Ops.open_idx(2),
		},
		{
			msg='x[1][1] for each',
			op=Ops.open_idx('1/1'),
		},
		{
			msg='[x[0], x[2]] for each',
			op=Ops.open_idx([0, 2]),
		},
		{
			msg='[x[0][2]] for each',
			op=Ops.open_idx([[0, 2]]),
		},
		{
			msg='[x[0], x[2], x[1][0], x[1][1]] for each',
			op=Ops.open_idx(['0', 2, '1/0', [1,1]]),
		}
	]

	for o in open_idx:
		printt(o.msg, chain.map(o.op, nested))
	
# ==============================================================	

func less_than_6(x):
	return x < 6

func plus_xy(x, y):
	return x+y

func mult_xyz(x, y, z):
	return x*y*z

class Div:
	extends Fko.Operators.OperatorBase
	var val
	
	func _init(val_=1):
		val = val_

	func eval(x):
		return float(x) / val
		
# ---------------------------------------------------------------------

var filter_ops = [
	{
		msg='x is even', 
		op=Ops.even()
	},
	{
		# can use Ops.odd() operator but for demonstration...
		msg='x is odd', 
		op=Ops.not_(Ops.even())
	},
	{
		# some Ops can wrap others	
		# and_ takes a list of any size
		# Ops.or_([Ops]) can also be called
		msg='4 < x <= 9', 
		op=Ops.and_([Ops.gt(4), Ops.lteq(9)])
	},
	{
		# use an expression operator
		# _x, _y are the default names for vars in Ops.expr
		# this can be changed at the top of Operators.gd
		msg='x != 4 and x != 1', 
		op=Ops.expr('_x != 4 and _x != 1')
	},
	{
		msg='x < 6', 
		op=Ops.func_(self, 'less_than_6')
	},
]
var map_ops = [
	{
		# identity returns the item as is
		msg='x', 
		op=Ops.identity()
	},
	{
		msg='x + 2', 
		op=Ops.expr('_x + 2')
	},
	{
		# call built-in function from expr
		msg='x cubed',
		op=Ops.expr('pow(_x, 3)')
	},
	{
		# named args can be passed to an expression
		msg='pow(x, y)',
		op=Ops.expr('pow(_x, y)', {y=10})
	},
	{
		# if-else
		msg='if x < 5: pow(x, 2) else: x * 10',
		op=Ops.if_(
			Ops.lt(5), 
			Ops.expr('pow(_x, 2)'), # then
			Ops.expr('_x * 10')) # else
	},
	{
		# additional args passed to function
		msg='x + y',
		op=Ops.func_(self, 'plus_xy', [2]),
	},
	{
		msg='x*y*z',
		op=Ops.func_(self, 'mult_xyz', [5, 10]),
	},
	{
		# use a class that derives from OperatorBase (^above)
		msg='user-op: x / 2',
		op=Div.new(2),
	},
	{
		# comp(ose) passes each entry through a series of operators
		msg='(((x + 3) + y) > 8)',
		op=Ops.comp([
			Ops.expr('_x + 3'),
			Ops.func_(self, 'plus_xy', [2])])
	},
	{
		# dict_apply returns a Dictionary
		#  it will create new fields if they do not exist (in the data)
		msg='dict_apply => {}\n',
		# an Array will be treated as Ops.comp()
		op=[Ops.expr('_x + 3'),
			Ops.dict_apply({value=Ops.identity(), is_even=Ops.even()})]
		},
	{
		msg='expr => {}\n',
		# if using '{}' in an expr use quotes outside and speech marks inside: 
		#   expr('{"key":value}')
		op=Ops.expr('{"value": _x}')
	},
]

# ---------------------------------------------------------------------
					
func operators():
	var data = range(10)
	
	prints('data:', data)
	ex_util.pr_break('=')
	
	printt('filter\n')
	
	var chain = Fko.chain()

	# filter returns all in data where the op returns true
	for f in filter_ops:
		printt(f.msg, chain.filter(f.op, data))
	ex_util.pr_break('=')

	printt('map\n')

	# map applies an op to each in data and returns the result
	for m in map_ops:
		printt(m.msg, chain.map(m.op, data))
	ex_util.pr_break('=')

	# ---------------------------------------------------------------------
	
	# ittr (iterator) passes the next item and previous result to the operator
	# in this case; adding them together
	# for this reason the function 'plus_xy' must take 2 args
	printt('add all items:',
		chain.ittr(Ops.func_(self, 'plus_xy'), data))

	# reduce returns the last result from ittr
	printt('product:',
		Fko.reduce(Ops.expr('_x * _y'), data.slice(1,-1)))

	# not all operators will work with sort, ittr, reduce
	# expr, func_, lt/gt/eq for this purpose
	ex_util.pr_break()
	
	print('Sorting\n')
	printt('sorted',
		Fko.sort(Ops.lt(), data))
	printt('sorted (descending)',
			chain.sort('_x > _y', data))
	printt('sorted dict',
		Fko.sort(
			Ops.expr('_x.x > _y.x'), 
			chain.map(Ops.expr('{"x":_x}'), data)))
