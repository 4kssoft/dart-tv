// Copyright (c) 2020, the Dart project authors. Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'package:analysis_server/lsp_protocol/protocol_generated.dart';
import 'package:analysis_server/lsp_protocol/protocol_special.dart';
import 'package:analysis_server/src/lsp/constants.dart';
import 'package:analysis_server/src/lsp/lsp_analysis_server.dart';

/// Helper for reading client dynamic registrations which may be ommitted by the
/// client.
class ClientDynamicRegistrations {
  /// All dynamic registrations supported by the Dart LSP server.
  ///
  /// Anything listed here and supported by the client will not send a static
  /// registration but intead dynamically register (usually only for a subset of
  /// files such as for .dart/pubspec.yaml/etc).
  ///
  /// When adding new capabilities that will be registered dynamically, the
  /// test_dynamicRegistration_XXX tests in `lsp/initialization_test.dart` should
  /// also be updated to ensure no double-registrations.
  static const supported = [
    Method.textDocument_didOpen,
    Method.textDocument_didChange,
    Method.textDocument_didClose,
    Method.textDocument_completion,
    Method.textDocument_hover,
    Method.textDocument_signatureHelp,
    Method.textDocument_references,
    Method.textDocument_documentHighlight,
    Method.textDocument_formatting,
    Method.textDocument_onTypeFormatting,
    Method.textDocument_definition,
    Method.textDocument_codeAction,
    Method.textDocument_rename,
    Method.textDocument_foldingRange,
  ];
  final ClientCapabilities _capabilities;

  ClientDynamicRegistrations(this._capabilities);

  bool get codeActions =>
      _capabilities.textDocument?.foldingRange?.dynamicRegistration ?? false;

  bool get completion =>
      _capabilities.textDocument?.completion?.dynamicRegistration ?? false;

  bool get definition =>
      _capabilities.textDocument?.definition?.dynamicRegistration ?? false;

  bool get documentHighlights =>
      _capabilities.textDocument?.documentHighlight?.dynamicRegistration ??
      false;

  bool get documentSymbol =>
      _capabilities.textDocument?.documentSymbol?.dynamicRegistration ?? false;

  bool get folding =>
      _capabilities.textDocument?.foldingRange?.dynamicRegistration ?? false;

  bool get formatting =>
      _capabilities.textDocument?.formatting?.dynamicRegistration ?? false;

  bool get hover =>
      _capabilities.textDocument?.hover?.dynamicRegistration ?? false;

  bool get implementation =>
      _capabilities.textDocument?.implementation?.dynamicRegistration ?? false;

  bool get references =>
      _capabilities.textDocument?.references?.dynamicRegistration ?? false;

  bool get rename =>
      _capabilities.textDocument?.rename?.dynamicRegistration ?? false;

  bool get signatureHelp =>
      _capabilities.textDocument?.signatureHelp?.dynamicRegistration ?? false;

  bool get textSync =>
      _capabilities.textDocument?.synchronization?.dynamicRegistration ?? false;

  bool get typeFormatting =>
      _capabilities.textDocument?.onTypeFormatting?.dynamicRegistration ??
      false;
}

class ServerCapabilitiesComputer {
  final LspAnalysisServer _server;

  /// Map from method name to current registration data.
  Map<String, Registration> _currentRegistrations = {};
  var _lastRegistrationId = 0;

  ServerCapabilitiesComputer(this._server);

  ServerCapabilities computeServerCapabilities(
      ClientCapabilities clientCapabilities) {
    final codeActionLiteralSupport =
        clientCapabilities.textDocument?.codeAction?.codeActionLiteralSupport;

    final renameOptionsSupport =
        clientCapabilities.textDocument?.rename?.prepareSupport ?? false;

    final dynamicRegistrations = ClientDynamicRegistrations(clientCapabilities);

    // When adding new capabilities to the server that may apply to specific file
    // types, it's important to update
    // [IntializedMessageHandler._performDynamicRegistration()] to notify
    // supporting clients of this. This avoids clients needing to hard-code the
    // list of what files types we support (and allows them to avoid sending
    // requests where we have only partial support for some types).
    return ServerCapabilities(
        dynamicRegistrations.textSync
            ? null
            : Either2<TextDocumentSyncOptions, num>.t1(TextDocumentSyncOptions(
                // The open/close and sync kind flags are registered dynamically if the
                // client supports them, so these static registrations are based on whether
                // the client supports dynamic registration.
                true,
                TextDocumentSyncKind.Incremental,
                false,
                false,
                null,
              )),
        dynamicRegistrations.hover ? null : true, // hoverProvider
        dynamicRegistrations.completion
            ? null
            : CompletionOptions(
                true, // resolveProvider
                dartCompletionTriggerCharacters,
              ),
        dynamicRegistrations.signatureHelp
            ? null
            : SignatureHelpOptions(
                dartSignatureHelpTriggerCharacters,
              ),
        dynamicRegistrations.definition ? null : true, // definitionProvider
        null,
        dynamicRegistrations.implementation
            ? null
            : true, // implementationProvider
        dynamicRegistrations.references ? null : true, // referencesProvider
        dynamicRegistrations.documentHighlights
            ? null
            : true, // documentHighlightProvider
        dynamicRegistrations.documentSymbol
            ? null
            : true, // documentSymbolProvider
        true, // workspaceSymbolProvider
        // "The `CodeActionOptions` return type is only valid if the client
        // signals code action literal support via the property
        // `textDocument.codeAction.codeActionLiteralSupport`."
        dynamicRegistrations.codeActions
            ? null
            : codeActionLiteralSupport != null
                ? Either2<bool, CodeActionOptions>.t2(
                    CodeActionOptions(DartCodeActionKind.serverSupportedKinds))
                : Either2<bool, CodeActionOptions>.t1(true),
        null,
        dynamicRegistrations.formatting
            ? null
            : true, // documentFormattingProvider
        false, // documentRangeFormattingProvider
        dynamicRegistrations.typeFormatting
            ? null
            : DocumentOnTypeFormattingOptions(
                dartTypeFormattingCharacters.first,
                dartTypeFormattingCharacters.skip(1).toList()),
        dynamicRegistrations.rename
            ? null
            : renameOptionsSupport
                ? Either2<bool, RenameOptions>.t2(RenameOptions(true))
                : Either2<bool, RenameOptions>.t1(true),
        null,
        null,
        dynamicRegistrations.folding ? null : true, // foldingRangeProvider
        null, // declarationProvider
        ExecuteCommandOptions(Commands.serverSupportedCommands),
        ServerCapabilitiesWorkspace(
            ServerCapabilitiesWorkspaceFolders(true, true)),
        null);
  }

  /// If the client supports dynamic registrations we can tell it what methods
  /// we support for which documents. For example, this allows us to ask for
  /// file edits for .dart as well as pubspec.yaml but only get hover/completion
  /// calls for .dart. This functionality may not be supported by the client, in
  /// which case they will use the ServerCapabilities to know which methods we
  /// support and it will be up to them to decide which file types they will
  /// send requests for.
  Future<void> performDynamicRegistration() async {
    final dartFiles = DocumentFilter('dart', 'file', null);
    final pubspecFile = DocumentFilter('yaml', 'file', '**/pubspec.yaml');
    final analysisOptionsFile =
        DocumentFilter('yaml', 'file', '**/analysis_options.yaml');

    final pluginTypes = _server.pluginManager.plugins
        .expand((plugin) => plugin.currentSession?.interestingFiles ?? const [])
        // All published plugins use something like `*.extension` as
        // interestingFiles. Prefix a `**/` so that the glob matches nested
        // folders as well.
        .map((glob) => DocumentFilter(null, 'file', '**/$glob'));

    final allTypes = [
      dartFiles,
      pubspecFile,
      analysisOptionsFile,
      ...pluginTypes
    ];

    final registrations = <Registration>[];

    /// Helper for creating registrations with IDs.
    void register(bool condition, Method method, [ToJsonable options]) {
      if (condition == true) {
        registrations.add(Registration(
            (_lastRegistrationId++).toString(), method.toJson(), options));
      }
    }

    final textCapabilities = _server.clientCapabilities?.textDocument;

    register(
      textCapabilities?.synchronization?.dynamicRegistration,
      Method.textDocument_didOpen,
      TextDocumentRegistrationOptions(allTypes),
    );
    register(
      textCapabilities?.synchronization?.dynamicRegistration,
      Method.textDocument_didClose,
      TextDocumentRegistrationOptions(allTypes),
    );
    register(
      textCapabilities?.synchronization?.dynamicRegistration,
      Method.textDocument_didChange,
      TextDocumentChangeRegistrationOptions(
          TextDocumentSyncKind.Incremental, allTypes),
    );
    register(
      _server.clientCapabilities?.textDocument?.completion?.dynamicRegistration,
      Method.textDocument_completion,
      CompletionRegistrationOptions(
        dartCompletionTriggerCharacters,
        null,
        true,
        [dartFiles],
      ),
    );
    register(
      textCapabilities?.hover?.dynamicRegistration,
      Method.textDocument_hover,
      TextDocumentRegistrationOptions([dartFiles]),
    );
    register(
      textCapabilities?.signatureHelp?.dynamicRegistration,
      Method.textDocument_signatureHelp,
      SignatureHelpRegistrationOptions(
          dartSignatureHelpTriggerCharacters, [dartFiles]),
    );
    register(
      _server.clientCapabilities?.textDocument?.references?.dynamicRegistration,
      Method.textDocument_references,
      TextDocumentRegistrationOptions([dartFiles]),
    );
    register(
      textCapabilities?.documentHighlight?.dynamicRegistration,
      Method.textDocument_documentHighlight,
      TextDocumentRegistrationOptions([dartFiles]),
    );
    register(
      textCapabilities?.documentSymbol?.dynamicRegistration,
      Method.textDocument_documentSymbol,
      TextDocumentRegistrationOptions([dartFiles]),
    );
    register(
      _server.clientCapabilities?.textDocument?.formatting?.dynamicRegistration,
      Method.textDocument_formatting,
      TextDocumentRegistrationOptions([dartFiles]),
    );
    register(
      textCapabilities?.onTypeFormatting?.dynamicRegistration,
      Method.textDocument_onTypeFormatting,
      DocumentOnTypeFormattingRegistrationOptions(
        dartTypeFormattingCharacters.first,
        dartTypeFormattingCharacters.skip(1).toList(),
        [dartFiles],
      ),
    );
    register(
      _server.clientCapabilities?.textDocument?.definition?.dynamicRegistration,
      Method.textDocument_definition,
      TextDocumentRegistrationOptions([dartFiles]),
    );
    register(
      textCapabilities?.implementation?.dynamicRegistration,
      Method.textDocument_implementation,
      TextDocumentRegistrationOptions([dartFiles]),
    );
    register(
      _server.clientCapabilities?.textDocument?.codeAction?.dynamicRegistration,
      Method.textDocument_codeAction,
      CodeActionRegistrationOptions(
          [dartFiles], DartCodeActionKind.serverSupportedKinds),
    );
    register(
      textCapabilities?.rename?.dynamicRegistration,
      Method.textDocument_rename,
      RenameRegistrationOptions(true, [dartFiles]),
    );
    register(
      textCapabilities?.foldingRange?.dynamicRegistration,
      Method.textDocument_foldingRange,
      TextDocumentRegistrationOptions(allTypes),
    );

    await _applyRegistrations(registrations);
  }

  Future<void> _applyRegistrations(List<Registration> registrations) async {
    final newRegistrationsByMethod = {
      for (final registration in registrations)
        registration.method: registration
    };

    final additionalRegistrations = List.of(registrations);
    final removedRegistrations = <Unregistration>[];

    // compute a diff of old and new registrations to send the unregister or
    // another register request. We assume that we'll only ever have one
    // registration per LSP method name.
    for (final entry in _currentRegistrations.entries) {
      final method = entry.key;
      final registration = entry.value;

      final newRegistrationForMethod = newRegistrationsByMethod[method];
      final entryRemovedOrChanged = newRegistrationForMethod?.registerOptions !=
          registration.registerOptions;

      if (entryRemovedOrChanged) {
        removedRegistrations
            .add(Unregistration(registration.id, registration.method));
      } else {
        additionalRegistrations.remove(newRegistrationForMethod);
      }
    }

    _currentRegistrations = newRegistrationsByMethod;

    if (removedRegistrations.isNotEmpty) {
      await _server.sendRequest(Method.client_unregisterCapability,
          UnregistrationParams(removedRegistrations));
    }

    // Only send the registration request if we have at least one (since
    // otherwise we don't know that the client supports registerCapability).
    if (additionalRegistrations.isNotEmpty) {
      final registrationResponse = await _server.sendRequest(
        Method.client_registerCapability,
        RegistrationParams(additionalRegistrations),
      );

      if (registrationResponse.error != null) {
        _server.logErrorToClient(
          'Failed to register capabilities with client: '
          '(${registrationResponse.error.code}) '
          '${registrationResponse.error.message}',
        );
      }
    }
  }
}
