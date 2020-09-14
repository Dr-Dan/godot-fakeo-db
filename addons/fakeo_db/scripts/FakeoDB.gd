extends Node

const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")
const Query = preload("res://addons/fakeo_db/scripts/Query.gd")
const Iterable = preload("res://addons/fakeo_db/scripts/Iterable.gd")

const Processors = preload("res://addons/fakeo_db/scripts/Processors.gd")
const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OperatorFactory = preload("res://addons/fakeo_db/scripts/OperatorFactory.gd")


static func in_(v, it) -> bool:
	return it.contains(v)
	
	
static func cltn(array:Array=[]) -> Enumerables.Collection:
	return Enumerables.Collection.new(array)
	

static func qry(query=[]) -> Query:
	return Query.new(_qry_to_arr(query))

static func itbl(query, coll) -> Iterable:
	return Iterable.new(_input_to_proc(query), coll)


static func reduce(op, coll):
	return ittr(op, coll).back()
	
static func ittr(op, coll):
	return itbl(Processors.IterateOp.new(op), coll)
	
static func mapq(op) -> Query:
	return qry([Processors.MapOp.new(op)])

static func filtq(op) -> Query:
	return qry([Processors.FilterOp.new(op)])


static func qapply(qry, coll) -> Array:
	return itbl(qry, coll).run()
	
static func mapply(input, coll) -> Array:
	return mapi(input, coll).run()
	
static func fapply(input, coll) -> Array:
	return filti(input, coll).run()
	

static func mapi(op, coll) -> Iterable:
	return itbl(Processors.MapOp.new(op), coll)

static func filti(op, coll) -> Iterable:
	return itbl(Processors.FilterOp.new(op), coll)

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
