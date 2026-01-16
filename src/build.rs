// Copyright (c) 2025 Redglyph (@gmail.com). All Rights Reserved.

use std::error::Error;
use std::fmt::{Display, Formatter};
use lexigram_core::log::{BufLog, LogReader, LogStatus};

// ---------------------------------------------------------------------------------------------

#[derive(Debug)]
pub enum BuildErrorSource {
    RuleTreeSet,
    Dfa,
    DfaBuilder,
    LexerGen,
    Lexi,
    ProdRuleSet,
    ParserGen,
    Gram,
    Lexigram,
}

#[derive(Debug)]
pub struct BuildError {
    log: BufLog,
    source: BuildErrorSource,
}

impl BuildError {
    pub fn new(log: BufLog, source: BuildErrorSource) -> Self {
        BuildError { log, source }
    }

    pub fn get_log(self) -> BufLog {
        self.log
    }
}

impl Display for BuildError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Errors have occurred in {:?}:\n{}", self.source, self.log.get_messages_str())
    }
}

impl Error for BuildError {
}

pub trait HasBuildErrorSource {
    const SOURCE: BuildErrorSource;

    fn get_build_error_source() -> BuildErrorSource {
        Self::SOURCE
    }
}

// ---------------------------------------------------------------------------------------------
// Local from/into and try_from/try_into
// - we have to redefine our own From/Into traits because the standard lib has a blanket
//   implementation that automatically generates TryFrom from From, which is always Ok...
// - we have to redefine our own TryFrom/TryInto traits, since it's otherwise not allowed to
//   implement a foreign trait on anything else than a local type (a local trait isn't enough)

pub trait BuildFrom<S>: Sized {
    /// Converts to this type from the input type.
    #[must_use]
    fn build_from(source: S) -> Self;
}

pub trait BuildInto<T>: Sized {
    /// Converts this type into the (usually inferred) input type.
    #[must_use]
    fn build_into(self) -> T;
}

impl<S, T> BuildInto<T> for S
where
    T: BuildFrom<S>,
{
    /// Calls `T::from(self)` to convert a [`S`] into a [`T`].
    #[inline]
    fn build_into(self) -> T { T::build_from(self) }
}

impl<S> BuildFrom<S> for S {
    fn build_from(source: S) -> Self {
        source
    }
}

// ---------------------------------------------------------------------------------------------------------

pub trait TryBuildFrom<T>: Sized {
    /// The type returned in the event of a conversion error.
    type Error;

    /// Performs the conversion.
    fn try_build_from(source: T) -> Result<Self, Self::Error>;
}

pub trait TryBuildInto<T>: Sized {
    /// The type returned in the event of a conversion error.
    type Error;

    /// Performs the conversion.
    fn try_build_into(self) -> Result<T, Self::Error>;
}

impl<S, T> TryBuildInto<T> for S
where
    T: TryBuildFrom<S>,
{
    type Error = T::Error;

    #[inline]
    fn try_build_into(self) -> Result<T, T::Error> { T::try_build_from(self) }
}

impl<S, T> TryBuildFrom<S> for T
where
    S: LogReader<Item = BufLog> + HasBuildErrorSource,
    T: LogReader<Item = BufLog> + BuildFrom<S> + HasBuildErrorSource,
{
    type Error = BuildError;

    fn try_build_from(source: S) -> Result<Self, Self::Error> {
        const VERBOSE: bool = false;
        if VERBOSE {
            println!("try_build_from <{}> -> <{}>: source messages\n{}",
                     std::any::type_name::<S>(), std::any::type_name::<T>(),
                     source.get_log().get_messages_str());
        }
        if source.get_log().has_no_errors() {
            let target = T::build_from(source);
            if VERBOSE {
                println!("try_build_from <{}> -> <{}>: target messages\n{}",
                         std::any::type_name::<S>(), std::any::type_name::<T>(),
                         target.get_log().get_messages_str());
            }
            if target.get_log().has_no_errors() {
                Ok(target)
            } else {
                Err(BuildError::new(target.give_log(), S::get_build_error_source()))
            }
        } else {
            Err(BuildError::new(source.give_log(), S::get_build_error_source()))
        }
    }
}
