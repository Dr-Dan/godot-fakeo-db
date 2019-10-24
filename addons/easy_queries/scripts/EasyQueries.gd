extends Node

const SCRIPTS_PATH = 'res://addons/easy_queries/scripts/'

const ed = SCRIPTS_PATH + "Enumerables.gd"
const qb = SCRIPTS_PATH + "QueryBuilder.gd"

const ops = SCRIPTS_PATH + "Operators.gd"
const of = SCRIPTS_PATH + "OperatorFactory.gd"

const Enumerables = preload(ed)
const QueryBuilder = preload(qb)

const Operators = preload(ops)
const OperatorFactory = preload(of)