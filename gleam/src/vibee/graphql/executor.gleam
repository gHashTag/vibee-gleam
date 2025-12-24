// GraphQL Executor - Query execution engine
// Executes GraphQL documents against resolver registry

import gleam/dict.{type Dict}
import gleam/io
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import vibee/graphql/types.{
  type Argument, type Context, type Document, type Field, type Operation,
  type OperationType, type Selection, type Value, Context, FieldSelection,
  Query, Mutation, Subscription,
}

pub type Resolver =
  fn(Dict(String, Value), Context) -> Result(Json, String)

pub type ResolverRegistry {
  ResolverRegistry(
    queries: Dict(String, Resolver),
    mutations: Dict(String, Resolver),
    subscriptions: Dict(String, Resolver),
  )
}

pub fn new_registry() -> ResolverRegistry {
  ResolverRegistry(
    queries: dict.new(),
    mutations: dict.new(),
    subscriptions: dict.new(),
  )
}

pub fn with_query(
  registry: ResolverRegistry,
  name: String,
  resolver: Resolver,
) -> ResolverRegistry {
  ResolverRegistry(
    ..registry,
    queries: dict.insert(registry.queries, name, resolver),
  )
}

pub fn with_mutation(
  registry: ResolverRegistry,
  name: String,
  resolver: Resolver,
) -> ResolverRegistry {
  ResolverRegistry(
    ..registry,
    mutations: dict.insert(registry.mutations, name, resolver),
  )
}

pub fn with_subscription(
  registry: ResolverRegistry,
  name: String,
  resolver: Resolver,
) -> ResolverRegistry {
  ResolverRegistry(
    ..registry,
    subscriptions: dict.insert(registry.subscriptions, name, resolver),
  )
}

pub fn execute(
  document: Document,
  variables: Dict(String, Value),
  operation_name: Option(String),
  registry: ResolverRegistry,
) -> types.GraphQLResponse {
  // Find the operation to execute
  let operation = find_operation(document.operations, operation_name)

  case operation {
    None -> types.error_response("No operation found")
    Some(op) -> {
      // Create execution context
      let ctx = Context(
        variables: variables,
        fragments: document.fragments,
        path: [],
      )

      // Get resolvers based on operation type
      let resolvers = case op.operation_type {
        Query -> registry.queries
        Mutation -> registry.mutations
        Subscription -> registry.subscriptions
      }

      // Execute selections
      execute_selections(op.selections, resolvers, ctx)
    }
  }
}

/// Find operation by name or return first one
fn find_operation(
  operations: List(Operation),
  name: Option(String),
) -> Option(Operation) {
  case name, operations {
    _, [] -> None
    None, [first, ..] -> Some(first)
    Some(target_name), [op, ..rest] -> {
      case op.name {
        Some(op_name) if op_name == target_name -> Some(op)
        _ -> find_operation(rest, name)
      }
    }
  }
}

/// Execute a list of selections and collect results
fn execute_selections(
  selections: List(Selection),
  resolvers: Dict(String, Resolver),
  ctx: Context,
) -> types.GraphQLResponse {
  // Log available resolvers for debugging
  let resolver_names = dict.keys(resolvers)
  let _ = io.println("[GraphQL] Available resolvers: " <> string.join(resolver_names, ", "))
  let _ = io.println("[GraphQL] Selections count: " <> string.inspect(list.length(selections)))

  let results = list.filter_map(selections, fn(sel) {
    case sel {
      FieldSelection(field) -> {
        let _ = io.println("[GraphQL] Executing field: " <> field.name)
        let result = execute_field(field, resolvers, ctx)
        case result {
          Ok(json_value) -> {
            let key = case field.alias {
              Some(alias) -> alias
              None -> field.name
            }
            let _ = io.println("[GraphQL] Field success: " <> key)
            Ok(#(key, json_value))
          }
          Error(err) -> {
            let _ = io.println("[GraphQL] Field error: " <> field.name <> " - " <> err)
            Error(Nil)
          }
        }
      }
      _ -> {
        let _ = io.println("[GraphQL] Skipping non-field selection")
        Error(Nil)
      }
    }
  })

  types.success_response(json.object(results))
}

/// Execute a single field
fn execute_field(
  field: Field,
  resolvers: Dict(String, Resolver),
  ctx: Context,
) -> Result(Json, String) {
  case dict.get(resolvers, field.name) {
    Error(_) -> Error("No resolver for field: " <> field.name)
    Ok(resolver) -> {
      // Convert arguments to Value dict
      let args = arguments_to_dict(field.arguments)

      // Call resolver
      resolver(args, ctx)
    }
  }
}

/// Convert list of Arguments to Dict
fn arguments_to_dict(arguments: List(Argument)) -> Dict(String, Value) {
  list.fold(arguments, dict.new(), fn(acc, arg) {
    dict.insert(acc, arg.name, arg.value)
  })
}

pub fn get_string_arg(args: Dict(String, Value), name: String) -> Option(String) {
  case dict.get(args, name) {
    Ok(types.StringValue(s)) -> Some(s)
    Ok(types.EnumValue(s)) -> Some(s)
    _ -> None
  }
}

pub fn require_string_arg(args: Dict(String, Value), name: String) -> Result(String, String) {
  case get_string_arg(args, name) {
    Some(s) -> Ok(s)
    None -> Error("Missing required argument: " <> name)
  }
}

pub fn get_int_arg(args: Dict(String, Value), name: String) -> Option(Int) {
  case dict.get(args, name) {
    Ok(types.IntValue(i)) -> Some(i)
    _ -> None
  }
}

pub fn require_int_arg(args: Dict(String, Value), name: String) -> Result(Int, String) {
  case get_int_arg(args, name) {
    Some(i) -> Ok(i)
    None -> Error("Missing required argument: " <> name)
  }
}

pub fn get_int_arg_with_default(args: Dict(String, Value), name: String, default: Int) -> Int {
  case get_int_arg(args, name) {
    Some(i) -> i
    None -> default
  }
}

pub fn get_bool_arg(args: Dict(String, Value), name: String) -> Option(Bool) {
  case dict.get(args, name) {
    Ok(types.BoolValue(b)) -> Some(b)
    _ -> None
  }
}
