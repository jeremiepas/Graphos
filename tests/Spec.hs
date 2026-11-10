-- | Full test runner for the `graphos-test` suite.
--
-- Originally driven by "-F -pgmF hspec-discover", a build-time GHC preprocessor
-- that scanned tests/ for *Spec.hs modules. That approach is fragile: it requires
-- the hspec-discover executable to be on PATH at the preprocessor step, but in this
-- project's build environment the exe only lives in the cabal store, so the step
-- failed ("Haskell pre-processor: could not execute: hspec-discover"). This explicit
-- runner mirrors the pattern already used by graphos-test-focus (FocusMain.hs): import
-- each declared spec module and run its "spec". The module list is kept in sync with
-- the cabal other-modules block of this suite.
module Main where

import qualified Test.Hspec
import qualified Graphos.Domain.TypesSpec
import qualified Graphos.Domain.SpecCheckSpec
import qualified Graphos.Domain.IntentGraphVerificationSpec
import qualified Graphos.Domain.ConfigSpec
import qualified Graphos.Domain.EmbeddingSpec
import qualified Graphos.Domain.Config.IngestSpec
import qualified Graphos.Domain.Config.SourceSpec
import qualified Graphos.Domain.Config.CacheSpec
import qualified Graphos.Domain.Config.MemorySpec
import qualified Graphos.Infrastructure.System.MemorySpec
import qualified Graphos.Domain.Types.IngestSpec
import qualified Graphos.Domain.GraphSpec
import qualified Graphos.Domain.CommunitySpec
import qualified Graphos.Domain.ModularityCrossCheckSpec
import qualified Graphos.Domain.Community.LabelSpec
import qualified Graphos.Domain.Community.ClusterInvarianceSpec
import qualified Graphos.Domain.Community.DeterminismSpec
import qualified Graphos.Domain.Community.PermutationSpec
import qualified Graphos.Domain.LabelingSpec
import qualified Graphos.Domain.AnalysisSpec
import qualified Graphos.Domain.ContextSpec
import qualified Graphos.UseCase.PipelineSpec
import qualified Graphos.UseCase.StagingSpec
import qualified Graphos.UseCase.ExtractSpec
import qualified Graphos.UseCase.InferSpec
import qualified Graphos.UseCase.InferDocumentSpec
import qualified Graphos.UseCase.DocLinkParitySpec
import qualified Graphos.UseCase.Extract.HaskellSpec
import qualified Graphos.UseCase.Extract.ImageSpec
import qualified Graphos.UseCase.QuerySpec
import qualified Graphos.UseCase.Query.RefineSpec
import qualified Graphos.UseCase.Query.RenderSpec
import qualified Graphos.UseCase.SelectContextSpec
import qualified Graphos.UseCase.SelectContextNoiseSpec
import qualified Graphos.UseCase.FormatContextSpec
import qualified Graphos.UseCase.FormatContextBudgetSpec
import qualified Graphos.UseCase.FormatContextHintsSpec
import qualified Graphos.UseCase.ConversationSpec
import qualified Graphos.UseCase.LoadSpec
import qualified Graphos.UseCase.ReportSpec
import qualified Graphos.UseCase.StartupValidationSpec
import qualified Graphos.Domain.Graph.ScoreSpec
import qualified Graphos.Domain.Graph.IndexSpec
import qualified Graphos.Domain.Graph.CollisionSpec
import qualified Graphos.Domain.Graph.NaturTransformationSpec
import qualified Graphos.Domain.Graph.SCCSpec
import qualified Graphos.Domain.Graph.StructuralSpec
import qualified Graphos.Domain.Graph.ColimitPropertySpec
import qualified Graphos.Domain.Graph.MergeConfluenceSpec
import qualified Graphos.Domain.Graph.ConflictSpec
import qualified Graphos.Domain.Graph.MergeIdempotencySpec
import qualified Graphos.Domain.Query.Cypher.MappingSpec
import qualified Graphos.Domain.Query.Cypher.ParserSpec
import qualified Graphos.Domain.Query.Cypher.EvalSpec
import qualified Graphos.Infrastructure.Extract.TreeSitter.ConvertSpec
import qualified Graphos.Infrastructure.Extract.TreeSitter.GrammarsSpec
import qualified Graphos.Infrastructure.Extract.TreeSitter.ImportsSpec
import qualified Graphos.Infrastructure.Extract.TreeSitterSpec
import qualified Graphos.Infrastructure.LSP.ClientSpec
import qualified Graphos.Infrastructure.LSP.TransportSpec
import qualified Graphos.Infrastructure.LLM.EmbeddingSpec
import qualified Graphos.Infrastructure.LLM.EmbeddingCacheSpec
import qualified Graphos.Infrastructure.Observability.SDKSpec
import qualified Graphos.Infrastructure.Server.MCPQuerySpec
import qualified Graphos.Infrastructure.Server.QueryAPISpec
import qualified Graphos.Infrastructure.FileSystem.OfficeConvertSpec
import qualified Graphos.Infrastructure.FileSystem.IgnoreSpec
import qualified Graphos.Infrastructure.FileSystem.AtomicWriteSpec
import qualified Graphos.Infrastructure.Scaffold.WriterSpec
import qualified Graphos.Domain.ScaffoldSpec
import qualified Graphos.UseCase.ScaffoldSpec
import qualified Graphos.CLI.ParserSpec
import qualified Graphos.Regression.ContextNoiseRegressionSpec
import qualified Graphos.Infrastructure.Export.HTMLSpec
import qualified Graphos.Infrastructure.Export.JGFSpec
import qualified Graphos.Infrastructure.Export.Neo4jSpec
import qualified Graphos.Infrastructure.Export.PersistMutationSpec
import qualified Graphos.Infrastructure.Export.ResearchHTMLSpec
import qualified Graphos.UseCase.SubgraphSpec
import qualified Graphos.UseCase.ResearchSpec
import qualified Graphos.UseCase.DetectSpec
import qualified Graphos.UseCase.DetectClassificationSpec
import qualified Graphos.Fidelity.ImportEdgesSpec
import qualified Graphos.Fidelity.GraphCoverageSpec
import qualified Graphos.UseCase.ExportSpec
import qualified Graphos.UseCase.PipelineIncrementalSpec
import qualified Graphos.UseCase.InferBetweennessWiringSpec
import qualified Graphos.UseCase.IngestSpec
import qualified Graphos.Infrastructure.FileSystem.CacheSpec
import qualified Graphos.UseCase.LabelCacheSpec
import qualified OpenSpecView.ChangeListSpec
import qualified OpenSpecView.ArtifactViewSpec
import qualified OpenSpecView.SpecDiffSpec
import qualified OpenSpecView.StateDashboardSpec
import qualified Graphos.UseCase.UpdateConfluenceSpec
import qualified Graphos.UseCase.ClusterOnlySpec
import qualified Graphos.UseCase.WatchCacheSpec
import qualified Graphos.Domain.Intent.ClassifierSpec
import qualified Graphos.Domain.Intent.ProjectionSpec
import qualified Graphos.Domain.Config.OutputSpec
import qualified Graphos.Infrastructure.FileSystem.OutputPlacementSpec
import qualified Graphos.Infrastructure.FileSystem.WatcherSpec

main :: IO ()
main = Test.Hspec.hspec $ do
  Graphos.Domain.TypesSpec.spec
  Graphos.Domain.SpecCheckSpec.spec
  Graphos.Domain.IntentGraphVerificationSpec.spec
  Graphos.Domain.ConfigSpec.spec
  Graphos.Domain.EmbeddingSpec.spec
  Graphos.Domain.Config.IngestSpec.spec
  Graphos.Domain.Config.SourceSpec.spec
  Graphos.Domain.Config.CacheSpec.spec
  Graphos.Domain.Config.MemorySpec.spec
  Graphos.Infrastructure.System.MemorySpec.spec
  Graphos.Domain.Types.IngestSpec.spec
  Graphos.Domain.GraphSpec.spec
  Graphos.Domain.CommunitySpec.spec
  Graphos.Domain.ModularityCrossCheckSpec.spec
  Graphos.Domain.Community.LabelSpec.spec
  Graphos.Domain.Community.ClusterInvarianceSpec.spec
  Graphos.Domain.Community.DeterminismSpec.spec
  Graphos.Domain.Community.PermutationSpec.spec
  Graphos.Domain.LabelingSpec.spec
  Graphos.Domain.AnalysisSpec.spec
  Graphos.Domain.ContextSpec.spec
  Graphos.UseCase.PipelineSpec.spec
  Graphos.UseCase.StagingSpec.spec
  Graphos.UseCase.ExtractSpec.spec
  Graphos.UseCase.InferSpec.spec
  Graphos.UseCase.InferDocumentSpec.spec
  Graphos.UseCase.DocLinkParitySpec.spec
  Graphos.UseCase.Extract.HaskellSpec.spec
  Graphos.UseCase.Extract.ImageSpec.spec
  Graphos.UseCase.QuerySpec.spec
  Graphos.UseCase.Query.RefineSpec.spec
  Graphos.UseCase.Query.RenderSpec.spec
  Graphos.UseCase.SelectContextSpec.spec
  Graphos.UseCase.SelectContextNoiseSpec.spec
  Graphos.UseCase.FormatContextSpec.spec
  Graphos.UseCase.FormatContextBudgetSpec.spec
  Graphos.UseCase.FormatContextHintsSpec.spec
  Graphos.UseCase.ConversationSpec.spec
  Graphos.UseCase.LoadSpec.spec
  Graphos.UseCase.ReportSpec.spec
  Graphos.UseCase.StartupValidationSpec.spec
  Graphos.Domain.Graph.ScoreSpec.spec
  Graphos.Domain.Graph.IndexSpec.spec
  Graphos.Domain.Graph.CollisionSpec.spec
  Graphos.Domain.Graph.NaturTransformationSpec.spec
  Graphos.Domain.Graph.SCCSpec.spec
  Graphos.Domain.Graph.StructuralSpec.spec
  Graphos.Domain.Graph.ColimitPropertySpec.spec
  Graphos.Domain.Graph.MergeConfluenceSpec.spec
  Graphos.Domain.Graph.ConflictSpec.spec
  Graphos.Domain.Graph.MergeIdempotencySpec.spec
  Graphos.Domain.Query.Cypher.MappingSpec.spec
  Graphos.Domain.Query.Cypher.ParserSpec.spec
  Graphos.Domain.Query.Cypher.EvalSpec.spec
  Graphos.Infrastructure.Extract.TreeSitter.ConvertSpec.spec
  Graphos.Infrastructure.Extract.TreeSitter.GrammarsSpec.spec
  Graphos.Infrastructure.Extract.TreeSitter.ImportsSpec.spec
  Graphos.Infrastructure.Extract.TreeSitterSpec.spec
  Graphos.Infrastructure.LSP.ClientSpec.spec
  Graphos.Infrastructure.LSP.TransportSpec.spec
  Graphos.Infrastructure.LLM.EmbeddingSpec.spec
  Graphos.Infrastructure.LLM.EmbeddingCacheSpec.spec
  Graphos.Infrastructure.Observability.SDKSpec.spec
  Graphos.Infrastructure.Server.MCPQuerySpec.spec
  Graphos.Infrastructure.Server.QueryAPISpec.spec
  Graphos.Infrastructure.FileSystem.OfficeConvertSpec.spec
  Graphos.Infrastructure.FileSystem.IgnoreSpec.spec
  Graphos.Infrastructure.FileSystem.AtomicWriteSpec.spec
  Graphos.Infrastructure.Scaffold.WriterSpec.spec
  Graphos.Domain.ScaffoldSpec.spec
  Graphos.UseCase.ScaffoldSpec.spec
  Graphos.CLI.ParserSpec.spec
  Graphos.Regression.ContextNoiseRegressionSpec.spec
  Graphos.Infrastructure.Export.HTMLSpec.spec
  Graphos.Infrastructure.Export.JGFSpec.spec
  Graphos.Infrastructure.Export.Neo4jSpec.spec
  Graphos.Infrastructure.Export.PersistMutationSpec.spec
  Graphos.Infrastructure.Export.ResearchHTMLSpec.spec
  Graphos.UseCase.SubgraphSpec.spec
  Graphos.UseCase.ResearchSpec.spec
  Graphos.UseCase.DetectSpec.spec
  Graphos.UseCase.DetectClassificationSpec.spec
  Graphos.Fidelity.ImportEdgesSpec.spec
  Graphos.Fidelity.GraphCoverageSpec.spec
  Graphos.UseCase.ExportSpec.spec
  Graphos.UseCase.PipelineIncrementalSpec.spec
  Graphos.UseCase.InferBetweennessWiringSpec.spec
  Graphos.UseCase.IngestSpec.spec
  Graphos.Infrastructure.FileSystem.CacheSpec.spec
  Graphos.UseCase.LabelCacheSpec.spec
  OpenSpecView.ChangeListSpec.spec
  OpenSpecView.ArtifactViewSpec.spec
  OpenSpecView.SpecDiffSpec.spec
  OpenSpecView.StateDashboardSpec.spec
  Graphos.UseCase.UpdateConfluenceSpec.spec
  Graphos.UseCase.ClusterOnlySpec.spec
  Graphos.UseCase.WatchCacheSpec.spec
  Graphos.Domain.Intent.ClassifierSpec.spec
  Graphos.Domain.Intent.ProjectionSpec.spec
  Graphos.Domain.Config.OutputSpec.spec
  Graphos.Infrastructure.FileSystem.OutputPlacementSpec.spec
  Graphos.Infrastructure.FileSystem.WatcherSpec.spec

