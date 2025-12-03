-module(wifi_scan).
-export([start/0]).

start() ->
    io:format("Starting WiFi Scan Example...~n"),
    
    %% Configure for STA mode. 
    %% We provide an empty SSID to initialize STA mode without connecting,
    %% which allows us to perform a scan immediately.
    Config = [
        {sta, [
            {ssid, ""},
            {psk, ""}
        ]}
    ],

    io:format("Starting network...~n"),
    case network:start(Config) of
        {ok, _Pid} ->
            io:format("Network started. Waiting a bit...~n"),
            timer:sleep(1000), %% Give it a moment to initialize
            
            io:format("Scanning for networks...~n"),
            try network:scan() of
                {ok, Results} ->
                    io:format("DEBUG: Got ok response from scan~n"),
                    try
                        Len = length(Results),
                        io:format("Scan complete. Found ~p networks.~n", [Len]),
                        % Just print the first one to be safe
                        case Results of
                            [First | _] -> 
                                io:format("First network: ~p~n", [First]);
                            [] -> 
                                io:format("No networks found~n")
                        end
                    catch
                        EC:EE:ES ->
                            io:format("Error processing results: ~p:~p~nStack: ~p~n", [EC, EE, ES])
                    end;
                {error, Reason} ->
                    io:format("Scan failed: ~p~n", [Reason]);
                Other ->
                    io:format("Scan returned unexpected: ~p~n", [Other])
            catch
                Class:Error:Stack ->
                    io:format("Scan crashed: ~p:~p~nStack: ~p~n", [Class, Error, Stack])
            end,
            
            io:format("Stopping network...~n"),
            % network:stop();
            io:format("Done.~n"),
            ok;
        Error ->
            io:format("Failed to start network: ~p~n", [Error]),
            {error, Error}
    end.
