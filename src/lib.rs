use std::{
    collections::BTreeSet,
    fmt::Display,
    result::Result as RawResult,
};
#[allow(unused_imports)]
use std::option::Option::{Some, None};
use RawResult::{Ok, Err};

pub type Result<T> = RawResult<T, (usize, &'static str)>;

#[derive(PartialEq)]
pub struct Error<'s> {
    src: &'s str,
    pub pos: usize,
    set: BTreeSet<&'static str>,
}
impl Error<'_> {
    pub fn tokens(&self) -> impl Iterator<Item = &'static str> + '_ {
        self.set.iter().copied()
    }
}
impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (line, column) = position(self.src, self.pos)
            .unwrap();
        let mut iter = self.set.iter();

        write!(f, "from {line}:{column} expected one of {}",
            iter.next().copied().unwrap_or("<uninit>"))?;
        for expect in iter {
            write!(f, ", {expect}")?;
        }
        Ok(())
    }
}
impl<'s> std::fmt::Debug for Error<'s> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Error")
            .field(&format_args!("`{self}`"))
            .finish()
    }
}

/// # Examples
/// ```
/// # use mini_macro_peg::position;
/// assert_eq!(position("", 0), Some((1, 1)));
/// assert_eq!(position("x", 0), Some((1, 1)));
/// assert_eq!(position("x", 1), Some((1, 2)));
/// assert_eq!(position("x\n", 1), Some((1, 2)));
/// assert_eq!(position("x\n", 2), Some((2, 1)));
/// assert_eq!(position("x\nx", 2), Some((2, 1)));
/// assert_eq!(position("x\nx", 3), Some((2, 2)));
/// assert_eq!(position("x\nx", 4), None);
/// ```
pub fn position(s: &str, index: usize) -> Option<(usize, usize)> {
    if index > s.len() { return None }

    let [mut line, mut column] = [0; 2];
    let mut newline = true;
    let mut f = |i, ch| {
        if newline {
            line += 1;
            column = 1;
            newline = false;
        } else {
            column += 1;
        }

        if i >= index {
            return Some((line, column));
        }

        if ch == '\n' {
            newline = true;
        }
        None
    };

    for (i, ch) in s.char_indices() {
        if let Some(x) = f(i, ch) {
            return Some(x);
        }
    }
    f(s.len(), ' ')
}

#[derive(Debug)]
pub struct Parser<'s> {
    src: &'s str,
    init_src: &'s str,
    max_err_pos: usize,
    errors: BTreeSet<&'static str>,
}
impl<'s> Parser<'s> {
    pub fn new(src: &'s str) -> Self {
        Self {
            src,
            init_src: src,
            max_err_pos: 0,
            errors: BTreeSet::new(),
        }
    }

    pub fn test<T, F>(&mut self, f: F) -> Result<T>
    where F: FnOnce(&mut Self) -> RawResult<T, &'static str>,
    {
        let back = self.location();
        self.test_rule(|this| f(this)
            .map_err(|e| (back, e)))
    }

    pub fn test_span<T, F>(&mut self, f: F) -> Result<&'s str>
    where F: FnOnce(&mut Self) -> Result<T>,
    {
        let back = self.location();
        let back_str = self.src;

        self.test_rule(f)?;

        let len = self.location() - back;
        Ok(&back_str[..len])
    }

    pub fn test_rule<T, F>(&mut self, f: F) -> Result<T>
    where F: FnOnce(&mut Self) -> Result<T>,
    {
        let prev = self.src;
        match f(self) {
            Ok(result) => {
                Ok(result)
            },
            Err(e @ (pos, expected)) => {
                if pos > self.max_err_pos {
                    self.max_err_pos = pos;
                    self.errors.clear();
                }
                self.errors.insert(expected);
                self.src = prev;
                Err(e)
            },
        }
    }

    pub fn test_optional_rule<T, F>(
        &mut self,
        f: F,
    ) -> Option<T>
    where F: FnOnce(&mut Self) -> Result<T>,
    {
        f(self).ok()
    }

    pub fn test_mult_rule<T, F>(
        &mut self,
        base: usize,
        to: Option<usize>,
        mut f: F,
    ) -> Result<Vec<T>>
    where F: FnMut(&mut Self) -> Result<T>,
    {
        let mut result = vec![];
        for _ in 0..base {
            result.push(self.test_rule(&mut f)?);
        }

        if let Some(to) = to {
            for _ in base..to {
                if let Ok(value) = self.test_rule(&mut f) {
                    result.push(value);
                } else {
                    break;
                }
            }
        } else {
            while let Ok(value) = self.test_rule(&mut f) {
                result.push(value);
            }
        }
        Ok(result)
    }

    pub fn test_source<T, F>(mut self, f: F) -> RawResult<T, Error<'s>>
    where F: FnOnce(&mut Self) -> Result<T>,
    {
        (|| {
            let result = self.test_rule(f)?;
            self.pat_eof()?;
            Result::Ok(result)
        })()
            .map_err(|_| {
                Error {
                    src: self.init_src,
                    pos: self.max_err_pos,
                    set: self.errors,
                }
            })
    }

    pub fn location(&self) -> usize {
        self.init_src.len() - self.src.len()
    }

    pub fn yes(&mut self, rng: usize) -> Option<&'s str> {
        let result = self.get(rng)?;
        self.src = &self.src[rng..];
        Some(result)
    }

    pub fn yes_ch(&mut self) -> Option<char> {
        let ch = self.src.chars().next()?;
        self.yes(ch.len_utf8())?;
        Some(ch)
    }

    pub fn get(&self, len: usize) -> Option<&'s str> {
        self.src.get(..len)
    }

    pub fn pat(&mut self, s: &str, name: &'static str)
    -> Result<&'s str> {
        self.test(|this| {
            if this.src.starts_with(s) {
                Ok(this.yes(s.len()).unwrap())
            } else {
                Err(name)
            }
        })
    }

    pub fn pat_eof(&mut self) -> Result<()> {
        self.test_rule(|this| {
            this.src.is_empty()
                .then_some(())
                .ok_or((this.location(), "EOF"))
        })
    }

    pub fn src(&self) -> &str {
        self.src
    }
}

#[macro_export]
macro_rules! rules_builder {
    ($self:ident<$input:lifetime> {
        $(
            $(#[$meta:meta])*
            $vis:vis rule $name:ident $(-> $res:ty)?
                = $pat:tt
        )*
    }) => {
        $(
            $(#[$meta])*
            $vis fn $name<$input>(
                $self: &mut $crate::Parser<$input>,
            ) -> $crate::Result<$crate::rules_builder!(@if $(($res))? else ())> {
                $crate::Ok($crate::rules_builder!(@expr($self) $pat))
            }
        )*
    };
    (@if ($($t:tt)*) else $($e:tt)*) => {
        $($t)*
    };
    (@if else $($e:tt)*) => {
        $($e)*
    };
    (@expr($self:ident) [$fst:tt / $($pat:tt)/+]) => {{
        $self.test_rule(|$self| {
            let mut result = (|| {
                $crate::Ok($crate::rules_builder!(@expr($self) $fst))
            })();

            $(
                result = result.or_else(|_| {
                    $crate::Ok($crate::rules_builder!(@expr($self) $pat))
                });
            )+

            result
        })?
    }};
    (@expr($self:ident) ($($name:ident),+ $(,)? : $pat:tt)) => {
        let value = $crate::rules_builder!(@expr($self) $pat);
        #[allow(unused_parens)]
        let ($($name),+) = value;
    };
    (@expr($self:ident) (.. $($pat:tt)*)) => {{
        $self.test_span(|$self| {
            $($crate::rules_builder!(@expr($self) $pat);)*
            $crate::Ok(())
        })?
    }};
    (@expr($self:ident) (%$res:block $($pat:tt)*)) => {{
        $self.test_rule(|$self| {
            $($crate::rules_builder!(@expr($self) $pat);)*
            (|| $crate::Ok($res))()
                .map_err(|e| ($self.location(), e))
        })?
    }};
    (@expr($self:ident) ($res:block $($pat:tt)*)) => {{
        $self.test_rule(|$self| {
            $($crate::rules_builder!(@expr($self) $pat);)*
            $crate::Ok($res)
        })?
    }};
    (@expr($self:ident) {$($num:literal)? $(, $to:literal)? * $pat:tt}) => {
        $self.test_mult_rule(
            0usize $(+ $num)?,
            $crate::None$(.unwrap_or($to))?,
            |$self| {
                $crate::Ok($crate::rules_builder!(@expr($self) $pat))
            }
        )?
    };
    (@expr($self:ident) {? $pat:tt}) => {
        $self.test_optional_rule(|$self| {
            $crate::Ok($crate::rules_builder!(@expr($self) $pat))
        })
};
    (@expr($self:ident) $lit:literal) => {{
        $self.pat($lit, ::std::stringify!($lit))?
    }};
    (@expr($self:ident) $rule:ident) => {{
        $self.test_rule($rule)?
    }};
    (@expr($self:ident) [@$pat:pat]) => {{
        $self.test(|$self| {
            if let $crate::Some(ch) = $self.yes_ch() {
                match ch {
                    $pat => $crate::Ok(ch),
                    _ => $crate::Err(::std::stringify!([$pat])),
                }
            } else {
                $crate::Err(::std::stringify!([$pat]))
            }
        })?
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        rules_builder!(this<'input> {
            pub rule number -> u64
                = (%{
                    s.parse().map_err(|_| "valid number")?
                } (s:["0" / (..{1*[@'0'..='9']})]))
            pub rule ws
                = ({} {*({} [@' ' | '\t' | '\r' | '\n'])})
            pub rule select -> u64
                = (%{
                    items.unwrap_or_default()
                        .get(n as usize)
                        .copied()
                        .ok_or("valid index select")?
                } "select" ws (n:expr) ws "["
                    ws (items:{? ({
                        let mut rest = rest;
                        rest.insert(0, fst);
                        rest
                    } (fst:expr)
                      (rest:{* ({ x } ws "," ws (x:expr))})
                      ws
                      {? ({} "," ws)}
                    )})
                "]")
            pub rule expr -> u64
                = [select / number]
            pub rule code -> u64
                = ({ x } ws (x:expr) ws)
        });

        let s = "
            select 0 [
                select 1 [
                    999,
                    233,
                ],
                999,
            ]
        ";
        assert_eq!(Parser::new(s).test_source(code), Ok(233));

        let s = "
            select select 0 [ 0 ] [
                select 1 [
                    999,
                    233,
                ],
                999,
            ]
        ";
        assert_eq!(Parser::new(s).test_source(code), Ok(233));
    }
}
