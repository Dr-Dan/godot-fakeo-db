tool
extends EditorScript

"""
To use: File > Run
(Ctrl+Shift+X)
"""

const ex_util = preload("res://addons/fakeo_db/examples/example_utils.gd")
const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory

# flex also supports chaining
func _run():
	# chains can be branched off and extended
	var map_mult_chn = fdb.flex().map('_x * 2')
	print(map_mult_chn.filter('_x >= 4', [1,2,3]))
		
	# As the ProcessorChainer is a Processor it can be passed to apply and itbl
	print(map_mult_chn.skip(1).map('float(_x) / 2', [1,2,3]))
		
	var map_pow_chn = map_mult_chn.map('pow(_x, 3)')

	# Chain()...procs...itbl(X) == fdb.itbl(procs, coll)
	printt('all:', map_pow_chn.itbl([1,2,3]).run())
	# use all the usual itbl functions
	printt('2nd:', map_pow_chn.itbl([1,2,3]).ffront())

	# bear in mind we are applying the chain from top to bottom
	#	to the data argument at the end
	print(fdb.flex()\
		.filter(ops.is_var(TYPE_ARRAY))\
		.map(ops.open_idx('1/0', 0))\
		.map('_x * 10',
			['hello world', [0, [1]], [1, 2, [3]], [3, [4]]]))

	# reduce requires evaluation so must be provided with a collection
	print(fdb.flex()\
		.filter('_x > 3')\
		.map('_x * 2')\
		.reduce(ops.expr('_x + _y'),
			[1,-2 ,12,6,1, 23]))
