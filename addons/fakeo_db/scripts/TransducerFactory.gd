extends Resource

const Tdxs = preload("res://addons/fakeo_db/scripts/Transducers.gd")
const Transducer = Tdxs.Transducer
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OpBase = Operators.OperatorBase
const OpFactory = preload("res://addons/fakeo_db/scripts/OperatorFactory.gd")

# -------------------------------------------------------

static func tdx()\
	-> Transducer:
	return Tdxs.Transducer.new()

static func comp(input:Array)\
	-> Transducer:
	return Tdxs.TdxIterator.new(input)

static func map(input)\
	-> Transducer:
	return Tdxs.MapOp.new(input)
	
static func filter(input)\
	-> Transducer:
	return Tdxs.FilterOp.new(input)

static func project(input)\
	-> Transducer:
	return map(OpFactory.open(input))

static func take(amt: int)\
	-> Transducer:
	return Tdxs.Take.new(amt)

static func take_while(op: OpBase)\
	-> Transducer:
	return Tdxs.TakeWhile.new(op)

static func skip(amt: int)\
	-> Transducer:
	return Tdxs.Skip.new(amt)

static func ittr(op)\
	-> Transducer:
	return Tdxs.IterateOp.new(op)

static func enumerate(key='i', step=1, wrap=false)\
	-> Transducer:
	return Tdxs.Enumerate.new(key, step, wrap)

static func as_args(fn:FuncRef)\
	-> Transducer:
	return map(Operators.FuncAsArgs.new(fn))
