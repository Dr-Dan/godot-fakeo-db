extends Node

const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")
const Query = preload("res://addons/fakeo_db/scripts/Query.gd")
const Iterable = preload("res://addons/fakeo_db/scripts/Iterable.gd")

const Processors = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OperatorFactory = preload("res://addons/fakeo_db/scripts/OperatorFactory.gd")

static func cltn(array:Array=[]) -> Enumerables.Collection:
	return Enumerables.Collection.new(array)
	

static func qry(query=[]) -> Query:
	return Query.new(_qry_to_arr(query))

static func iter(coll, query=[]) -> Iterable:
	# TODO: should remove this
	# if coll is Iterable:
	# 	coll = coll.run()
	return Iterable.new(_input_to_proc(query), coll)

static func in_(v, it) -> bool:
	return it.contains(v)
	
static func reduce(coll, op, args=[]):
	if op is FuncRef:
		op = Operators.Func.new(op, args)
	elif op is String:
		op = Operators.Expr.new(op)
#	elif not (op is Operators.Expr or op is Operators.Func):
#		push_warning('reduce should be called with: String, FuncRef, Operators.Expr, Operators.Func')
	# return iter(coll, qry().reduce(op)).back()
	return iter(coll, Processors.IterateOp.new(op)).back()


static func mapq(op, args=[]) -> Query:
	return qry([Query.Proc.MapOpAuto.new(op, args)])

static func filtq(op, args=[]) -> Query:
	return qry([Query.Proc.FilterOpAuto.new(op, args)])


static func qapply(coll, qry) -> Array:
	return iter(coll, qry).run()
	
static func mapply(coll, input, args=[]) -> Array:
	return mapi(coll, input, args).run()
	
static func fapply(coll, input, args=[]) -> Array:
	return filti(coll, input, args).run()
	

static func mapi(coll, op, args=[]) -> Iterable:
	return iter(coll, Processors.MapOpAuto.new(op, args))

static func filti(coll, op, args=[]) -> Iterable:
	return iter(coll, Processors.FilterOpAuto.new(op, args))


static func _input_to_proc(query) -> Processors.Processor:
	if query is Iterable or query is Query:
		return Processors.ProcIterator.new(query.items)
	elif query is Array:
		return Processors.ProcIterator.new(query)
	assert(query is Processors.Processor)
	return query
		
static func _qry_to_arr(query) -> Array:
	if query is Iterable:
		return query.proc.procs
	elif query is Query:
		return [] + query.items
	elif query is Processors.Processor:
		return [query]
	assert(query is Array)
	return query
