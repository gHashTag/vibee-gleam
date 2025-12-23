// GraphQL Types
// Core type definitions for GraphQL implementation

import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}

// === REQUEST/RESPONSE TYPES ===

pub type GraphQLRequest {
  GraphQLRequest(
    query: String,
    operation_name: Option(String),
    variables: Dict(String, Json),
  )
}

pub type GraphQLResponse {
  GraphQLResponse(data: Option(Json), errors: Option(List(GraphQLError)))
}

pub type GraphQLError {
  GraphQLError(
    message: String,
    locations: Option(List(Location)),
    path: Option(List(String)),
    extensions: Option(Json),
  )
}

pub type Location {
  Location(line: Int, column: Int)
}

// === DOCUMENT TYPES ===

pub type Document {
  Document(operations: List(Operation), fragments: Dict(String, Fragment))
}

pub type Operation {
  Operation(
    operation_type: OperationType,
    name: Option(String),
    variables: List(VariableDefinition),
    directives: List(Directive),
    selections: List(Selection),
  )
}

pub type OperationType {
  Query
  Mutation
  Subscription
}

pub type Fragment {
  Fragment(
    name: String,
    type_condition: String,
    directives: List(Directive),
    selections: List(Selection),
  )
}

pub type VariableDefinition {
  VariableDefinition(
    name: String,
    var_type: TypeRef,
    default_value: Option(Value),
  )
}

pub type TypeRef {
  NamedType(name: String)
  ListType(inner: TypeRef)
  NonNullType(inner: TypeRef)
}

pub type Directive {
  Directive(name: String, arguments: List(Argument))
}

// === SELECTION SET ===

pub type Selection {
  FieldSelection(field: Field)
  FragmentSpread(name: String, directives: List(Directive))
  InlineFragment(
    type_condition: Option(String),
    directives: List(Directive),
    selections: List(Selection),
  )
}

pub type Field {
  Field(
    alias: Option(String),
    name: String,
    arguments: List(Argument),
    directives: List(Directive),
    selections: List(Selection),
  )
}

pub type Argument {
  Argument(name: String, value: Value)
}

// === VALUE TYPES ===

pub type Value {
  IntValue(Int)
  FloatValue(Float)
  StringValue(String)
  BoolValue(Bool)
  NullValue
  EnumValue(String)
  ListValue(List(Value))
  ObjectValue(Dict(String, Value))
  VariableRef(String)
}

// === CONTEXT ===

pub type Context {
  Context(
    variables: Dict(String, Value),
    fragments: Dict(String, Fragment),
    path: List(String),
  )
}

pub fn empty_context() -> Context {
  Context(variables: dict.new(), fragments: dict.new(), path: [])
}

// === HELPER FUNCTIONS ===

pub fn error_response(message: String) -> GraphQLResponse {
  GraphQLResponse(
    data: None,
    errors: Some([
      GraphQLError(message: message, locations: None, path: None, extensions: None),
    ]),
  )
}

pub fn success_response(data: Json) -> GraphQLResponse {
  GraphQLResponse(data: Some(data), errors: None)
}
