namespace TDesu.Telegram.TL.Tests

open NUnit.Framework

[<AutoOpen>]
module TestHelpers =

    /// Assert two items are equal (resolves NUnit overload ambiguity)
    let inline equals<'T> (actual: 'T) (expected: 'T) =
        Assert.That(actual, Is.EqualTo(expected), NUnitString.op_Implicit "items not equal")

    /// Assert two items are not equal
    let inline notEquals<'T> (actual: 'T) (expected: 'T) =
        Assert.That(actual, Is.Not.EqualTo(expected), NUnitString.op_Implicit "items should not be equal")
