pub struct Json<S>(pub S);

pub struct JsonAny;
pub struct JsonBool;
pub struct JsonString;
pub struct JsonNumber;
pub struct JsonArray<T>(pub T);

// Avoids a panic in the compiler if we have &'static [u8] directly in a slice in a constant.
#[derive(PartialEq, Eq)]
pub enum StringList {
    Cons(&'static [u8], &'static StringList),
    Nil
}

pub struct JsonStringEnum<const MAX : usize, const STRS : &'static StringList>;

pub use paste::paste;
#[macro_export]
macro_rules! define_json_struct {
    { $name:ident $n:literal { $($field:ident : $schemaType:ty),* } } => {

        $crate::json::paste! {
#[derive(Default, Debug, PartialEq, Clone)]
            pub struct $name<$([<Field $field:camel>]),*> {
                $(pub [<field_ $field:snake>] : [<Field $field:camel>] ),*
            }

            pub type [<$name Schema>] = $name<$($schemaType),*>;
        }
    }
}
