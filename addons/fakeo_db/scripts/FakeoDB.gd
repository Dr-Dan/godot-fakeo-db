extends Node

const Enumerables = preload("res://addons/fakeo_db/scripts/Enumerables.gd")
const Query = preload("res://addons/fakeo_db/scripts/Query.gd")
const QueryIterable = preload("res://addons/fakeo_db/scripts/QueryIterable.gd")
const Iterable = preload("res://addons/fakeo_db/scripts/Iterable.gd")

const Operators = preload("res://addons/fakeo_db/scripts/Operators.gd")
const OperatorFactory = preload("res://addons/fakeo_db/scripts/OperatorFactory.gd")

static func cltn(array:Array=[]) -> Enumerables.Collection:
	return Enumerables.Collection.new(array)
	

static func qry(query=[]) -> Query:
	return Query.new(_qry_to_arr(query))

static func iter(coll, query=[]) -> Iterable:
	return Iterable.new(_qry_to_arr(query), coll)

static func qry_iter(coll, query=[]) -> QueryIterable:		
	return QueryIterable.new(coll, _qry_to_arr(query))	

static func apply(coll, qry):
	return iter(coll, qry).run()

static func for_each(coll, op, args=[]) -> Array:
	return qry().map(op, args).apply(coll)	
	
static func mapq(input, args=[]) -> Query:
	return qry([Query.Proc.MapOpAuto.new(input, args)])

static func filtq(input, args=[]) -> Query:
	return qry([Query.Proc.FilterOpAuto.new(input, args)])
	

static func mapi(coll, input, args=[]) -> Iterable:
	return iter(coll, [Query.Proc.MapOpAuto.new(input, args)])

static func filti(coll, input, args=[]) -> Iterable:
	return iter(coll, [Query.Proc.FilterOpAuto.new(input, args)])

static func mapqi(coll, input, args=[]) -> QueryIterable:
	return qry_iter(coll, [Query.Proc.MapOpAuto.new(input, args)])

static func filtqi(coll, input, args=[]) -> QueryIterable:
	return qry_iter(coll, [Query.Proc.FilterOpAuto.new(input, args)])


static func _qry_to_arr(query):
	if query is Iterable:
		return query.query
	elif query is Query:
		return query.items
	assert(query is Array)
	return query
