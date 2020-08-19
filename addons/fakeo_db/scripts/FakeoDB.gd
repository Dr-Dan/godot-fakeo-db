extends Node

const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")
const Query = preload("res://addons/fakeo_db/scripts/Query.gd")
const QueryIterable = preload("res://addons/fakeo_db/scripts/QueryIterable.gd")
const Iterable = preload("res://addons/fakeo_db/scripts/Iterable.gd")

const Processors = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OperatorFactory = preload("res://addons/fakeo_db/scripts/OperatorFactory.gd")

static func cltn(array:Array=[]) -> Enumerables.Collection:
	return Enumerables.Collection.new(array)
	

static func qry(query=[]) -> Query:
	return Query.new(_qry_to_arr(query))

static func iter(coll, query=[]) -> Iterable:
	if coll is Iterable:
		coll = coll.run()
	return Iterable.new(_qry_to_arr(query), coll)

static func apply(coll, qry) -> Array:
	return iter(coll, qry).run()

static func in_(v, it) -> bool:
	return it.contains(v)
	
static func reduce(coll, op, args=[]):
	if op is FuncRef:
		op = Operators.Func.new(op, args)
	elif op is String:
		op = Operators.Expr.new(op)	
#	elif not (op is Operators.Expr or op is Operators.Func):
#		push_warning('reduce should be called with: String, FuncRef, Operators.Expr, Operators.Func')
#	assert(op is Operators.Func or op is Operators.Expr)
	return iter(coll, qry().reduce(op)).back()
	
static func mapply(coll, input, args=[]) -> Array:
	return mapi(coll, input, args).run()
	
static func fapply(coll, input, args=[]) -> Array:
	return filti(coll, input, args).run()	
	
static func mapq(input, args=[]) -> Query:
	return qry([Query.Proc.MapOpAuto.new(input, args)])

static func filtq(input, args=[]) -> Query:
	return qry([Query.Proc.FilterOpAuto.new(input, args)])
	

static func mapi(coll, input, args=[]) -> Iterable:
	return iter(coll, [Query.Proc.MapOpAuto.new(input, args)])

static func filti(coll, input, args=[]) -> Iterable:
	return iter(coll, [Query.Proc.FilterOpAuto.new(input, args)])


static func _qry_to_arr(query) -> Array:
	if query is Iterable:
		return query.query
	elif query is Query:
		return [] + query.items
	elif query is Processors.Processor:
		return [query]
	assert(query is Array)
	return query
