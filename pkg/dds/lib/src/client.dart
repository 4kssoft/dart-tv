// Copyright (c) 2020, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

part of dds;

/// Representation of a single DDS client which manages the connection and
/// DDS request intercepting / forwarding.
class _DartDevelopmentServiceClient {
  _DartDevelopmentServiceClient(
    this.dds,
    this.ws,
    json_rpc.Peer vmServicePeer,
  ) : _vmServicePeer = vmServicePeer {
    _clientPeer = json_rpc.Peer(ws.cast<String>());
    _registerJsonRpcMethods();
  }

  /// Start receiving JSON RPC requests from the client.
  ///
  /// Returned future completes when the peer is closed.
  Future<void> listen() => _clientPeer.listen().then(
        (_) => dds.streamManager.clientDisconnect(this),
      );

  /// Close the connection to the client.
  Future<void> close() async {
    // Cleanup the JSON RPC server for this connection if DDS has shutdown.
    await _clientPeer.close();
  }

  /// Send a JSON RPC notification to the client.
  void sendNotification(String method, [dynamic parameters]) {
    if (_clientPeer.isClosed) {
      return;
    }
    _clientPeer.sendNotification(method, parameters);
  }

  /// Send a JSON RPC request to the client.
  Future<dynamic> sendRequest(String method, [dynamic parameters]) async {
    if (_clientPeer.isClosed) {
      return null;
    }
    return await _clientPeer.sendRequest(method, parameters);
  }

  /// Registers handlers for JSON RPC methods which need to be intercepted by
  /// DDS as well as fallback request forwarder.
  void _registerJsonRpcMethods() {
    _clientPeer.registerMethod('streamListen', (parameters) async {
      final streamId = parameters['streamId'].asString;
      await dds.streamManager.streamListen(this, streamId);
      return _success;
    });

    _clientPeer.registerMethod('streamCancel', (parameters) async {
      final streamId = parameters['streamId'].asString;
      await dds.streamManager.streamCancel(this, streamId);
      return _success;
    });

    _clientPeer.registerMethod('registerService', (parameters) async {
      final serviceId = parameters['service'].asString;
      final alias = parameters['alias'].asString;
      if (services.containsKey(serviceId)) {
        throw _RpcErrorCodes.buildRpcException(
          _RpcErrorCodes.kServiceAlreadyRegistered,
        );
      }
      services[serviceId] = alias;
      // Notify other clients that a new service extension is available.
      dds.streamManager.sendServiceRegisteredEvent(
        this,
        serviceId,
        alias,
      );
      return _success;
    });

    // When invoked within a fallback, the next fallback will start executing.
    // The final fallback forwards the request to the VM service directly.
    @alwaysThrows
    nextFallback() => throw json_rpc.RpcException.methodNotFound('');

    // Handle service extension invocations.
    _clientPeer.registerFallback((parameters) async {
      hasNamespace(String method) => method.contains('.');
      getMethod(String method) => method.split('.').last;
      getNamespace(String method) => method.split('.').first;
      if (!hasNamespace(parameters.method)) {
        nextFallback();
      }
      // Lookup the client associated with the service extension's namespace.
      // If the client exists and that client has registered the specified
      // method, forward the request to that client.
      final method = getMethod(parameters.method);
      final namespace = getNamespace(parameters.method);
      final serviceClient = dds._clients[namespace];
      if (serviceClient != null && serviceClient.services.containsKey(method)) {
        return await Future.any(
          [
            // Forward the request to the service client or...
            serviceClient.sendRequest(method, parameters.asMap),
            // if the service client closes, return an error response.
            serviceClient._clientPeer.done.then(
              (_) => throw _RpcErrorCodes.buildRpcException(
                _RpcErrorCodes.kServiceDisappeared,
              ),
            ),
          ],
        );
      }
      throw json_rpc.RpcException(
        _RpcErrorCodes.kMethodNotFound,
        'Unknown service: ${parameters.method}',
      );
    });

    // Unless otherwise specified, the request is forwarded to the VM service.
    // NOTE: This must be the last fallback registered.
    _clientPeer.registerFallback((parameters) async =>
        await _vmServicePeer.sendRequest(parameters.method, parameters.asMap));
  }

  static const _success = <String, dynamic>{
    'type': 'Success',
  };

  final _DartDevelopmentService dds;
  final Map<String, String> services = {};
  final json_rpc.Peer _vmServicePeer;
  final WebSocketChannel ws;
  json_rpc.Peer _clientPeer;
}
