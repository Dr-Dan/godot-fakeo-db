extends Resource

const Procs = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Processor = Procs.Processor
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OpBase = Operators.OperatorBase

# -------------------------------------------------------
static func proc()\
	-> Processor:
	return Procs.Processor.new()

static func comp(input:Array)\
	-> Processor:
	return Procs.ProcIterator.new(input)

static func map(input)\
	-> Processor:
	return Procs.MapOp.new(input)
	# return Procs.MapOp.new(Operators.Util.get_map_op(input))
	
static func filter(input)\
	-> Processor:
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
	-> Processor:
	return Procs.Take.new(amt)

static func take_while(op: OpBase)\
	-> Processor:
	return Procs.TakeWhile.new(op)

static func skip(amt: int)\
	-> Processor:
	return Procs.Skip.new(amt)

static func ittr(op)\
	-> Processor:
	return Procs.IterateOp.new(op)

static func enumerate(key='i', step=1, wrap=false)\
	-> Processor:
	return Procs.Enumerate.new(key, step, wrap)

static func as_args(fn:FuncRef)\
	-> Processor:
	return map(Operators.FuncAsArgs.new(fn))
