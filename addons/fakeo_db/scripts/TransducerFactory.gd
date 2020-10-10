extends Resource

const Procs = preload("res://addons/fakeo_db/scripts/Transducers.gd")
const Transducer = Procs.Transducer
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OpBase = Operators.OperatorBase

# -------------------------------------------------------
# static func proc()\
# 	-> Transducer:
# 	return Procs.Transducer.new()

static func comp(input:Array)\
	-> Transducer:
	return Procs.ProcIterator.new(input)

static func map(input)\
	-> Transducer:
	return Procs.MapOp.new(input)
	# return Procs.MapOp.new(Operators.Util.get_map_op(input))
	
static func filter(input)\
	-> Transducer:
	return Procs.FilterOp.new(input)
	# return Procs.FilterOp.new(Operators.Util.get_filter_op(input))

# TODO: use op factory for this
static func project(input):
	var op = null
	if input is Array:
		op = Operators.OpenMultiDeep.new(input)
	else:
		assert(input is String) 
		op = Operators.OpenDeep.new(input)
	return map(op)

static func take(amt: int)\
	-> Transducer:
	return Procs.Take.new(amt)

static func take_while(op: OpBase)\
	-> Transducer:
	return Procs.TakeWhile.new(op)

static func skip(amt: int)\
	-> Transducer:
	return Procs.Skip.new(amt)

static func ittr(op)\
	-> Transducer:
	return Procs.IterateOp.new(op)

static func enumerate(key='i', step=1, wrap=false)\
	-> Transducer:
	return Procs.Enumerate.new(key, step, wrap)

static func as_args(fn:FuncRef)\
	-> Transducer:
	return map(Operators.FuncAsArgs.new(fn))
