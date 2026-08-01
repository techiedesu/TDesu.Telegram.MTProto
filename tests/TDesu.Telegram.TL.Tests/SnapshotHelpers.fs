namespace TDesu.Telegram.TL.Tests

open System.IO
open NUnit.Framework

[<AutoOpen>]
module SnapshotHelpers =

    let private snapshotDir = Path.Combine(__SOURCE_DIRECTORY__, "Snapshots")
    let private testDataDir = Path.Combine(__SOURCE_DIRECTORY__, "TestData")

    /// Trim trailing whitespace at the end of the whole file, and NOTHING
    /// else.
    ///
    /// This used to `Replace("\r\n", "\n")` first, and that is *structurally*
    /// why the `Environment.NewLine` defect survived a year: every snapshot
    /// assertion in this suite was blind to the one difference the emitters
    /// actually had between machines. The line-ending policy is now part of
    /// what a snapshot pins — the emitters commit to LF (EmitCSharp's
    /// `NormalizeWhitespace(_, "\n")`, Fantomas `EndOfLineStyle.LF`,
    /// `Append('\n')` in EmitTemplates), so a CRLF in the output is a real
    /// regression and has to be able to fail.
    ///
    /// The `.expected` files are therefore stored LF and this repository's
    /// `.gitattributes` marks them `-text` so no checkout rewrites them.
    let private normalize (s: string) = s.TrimEnd()

    /// Replace non-deterministic timestamps in generated code.
    let normalizeTimestamp (code: string) =
        code.Split('\n')
        |> Array.map (fun line ->
            if line.TrimStart().StartsWith("// Auto-generated at") then
                "// Auto-generated at [TIMESTAMP]"
            else
                line)
        |> String.concat "\n"

    /// Read a file from the TestData directory.
    let readTestData (fileName: string) =
        File.ReadAllText(Path.Combine(testDataDir, fileName))

    /// Assert that actual output matches the golden snapshot file.
    ///
    /// A missing snapshot FAILS. It used to write the file and pass, which
    /// makes a renamed or deleted `.expected` a self-fulfilling green:
    /// whatever the emitter produced on that run silently becomes the
    /// reference forever. A golden file is a human deciding the output is
    /// correct — never the assertion that is supposed to check it.
    /// `updateSnapshot` is the deliberate way to (re)create one.
    let assertMatchesSnapshot (actual: string) (snapshotName: string) =
        let expectedPath = Path.Combine(snapshotDir, $"{snapshotName}.expected")
        let actualPath = Path.Combine(snapshotDir, $"{snapshotName}.actual")
        let normalizedActual = normalize actual

        if not (File.Exists expectedPath) then
            if not (Directory.Exists snapshotDir) then
                Directory.CreateDirectory snapshotDir |> ignore
            File.WriteAllText(actualPath, normalizedActual)

            Assert.Fail
                $"Missing snapshot: {snapshotName}.expected\n\n\
                  This run's output was written to {snapshotName}.actual. Read it, decide it is\n\
                  correct, and copy it over the .expected — do not let the assertion create its\n\
                  own reference."
        else
            let expected = normalize (File.ReadAllText expectedPath)

            if normalizedActual <> expected then
                File.WriteAllText(actualPath, normalizedActual)
                let el = expected.Split('\n')
                let al = normalizedActual.Split('\n')
                let maxLen = max el.Length al.Length
                let diffs = ResizeArray<string>()

                for i in 0 .. maxLen - 1 do
                    let e =
                        if i < el.Length then
                            el[i]
                        else
                            "<EOF>"

                    let a =
                        if i < al.Length then
                            al[i]
                        else
                            "<EOF>"

                    if e <> a && diffs.Count < 15 then
                        diffs.Add $"L{i + 1}:\n  - {e}\n  + {a}"

                let diffText = String.concat "\n" diffs

                // Two files that differ only in `\r` produce a diff where
                // every line looks identical. Say what actually happened.
                let sameIgnoringEol =
                    expected.Replace("\r\n", "\n") = normalizedActual.Replace("\r\n", "\n")

                if sameIgnoringEol then
                    let crlfIn (s: string) = s.Split('\n') |> Array.filter (fun l -> l.EndsWith "\r") |> Array.length

                    Assert.Fail
                        $"Snapshot line endings differ: {snapshotName}\n\n\
                          expected has {crlfIn expected} CRLF line(s), emitted output has \
                          {crlfIn normalizedActual}.\n\
                          The emitters commit to LF on every platform; a CRLF here is a real \
                          regression,\nnot a checkout artefact (.gitattributes marks .expected \
                          binary)."
                else
                    Assert.Fail $"Snapshot mismatch: {snapshotName}\n\n{diffText}"
            elif File.Exists actualPath then
                File.Delete actualPath

    /// Force-update a snapshot file.
    let updateSnapshot (content: string) (snapshotName: string) =
        if not (Directory.Exists snapshotDir) then
            Directory.CreateDirectory snapshotDir |> ignore

        let expectedPath = Path.Combine(snapshotDir, $"{snapshotName}.expected")
        File.WriteAllText(expectedPath, normalize content)
