package tests

import (
	"fmt"
	"net"
	"net/netip"
	"os"
	"os/exec"
	"reflect"
	"testing"
)

var bind = netip.MustParseAddrPort("0.0.0.0:60000")

func TestMain(m *testing.M) {
	socket, err := setup()

	if err != nil {
		fmt.Printf("   *** ERROR %v", err)
		os.Exit(1)
	} else {
		code := m.Run()
		teardown(socket)

		os.Exit(code)
	}
}

func setup() (*net.UDPConn, error) {
	bindTo := net.UDPAddrFromAddrPort(bind)

	if socket, err := net.ListenUDP("udp", bindTo); err != nil {
		return nil, err
	} else {

		go func() {
			buffer := make([]byte, 1024)

			for {
				if N, addr, err := socket.ReadFromUDP(buffer); err != nil {
					break
				} else if N == 64 {
					request := buffer[0:N]

					for k, v := range requests {
						if reflect.DeepEqual(request, v) {
							if response, ok := responses[k]; ok {
								socket.WriteToUDP(response, addr)
							}
						}
					}
				}
			}
		}()
		return socket, nil
	}
}

func teardown(socket *net.UDPConn) {
	socket.Close()
}

// func TestCLI(t *testing.T) {
// 	cmd := exec.Command("../../uhppote-cli/bin/uhppote-cli", "get-device", "405419896")
// 	out, err := cmd.CombinedOutput()
// 	if err != nil {
// 		t.Fatalf("Error executing CLI get-device (%v)", err)
// 	} else if string(out) != "Alpha  405419896  192.168.1.100   255.255.255.0   192.168.1.1     00:12:23:34:45:56 v8.92 2018-11-05\n" {
// 		t.Errorf("   > %v", string(out))
// 	}
// }

func TestPython(t *testing.T) {
	cmd := exec.Command("/bin/sh", "-c", "cd python && make debug")

	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("Error executing Python get-device (%v)", err)
	} else {
		t.Errorf(">>>>>>>>>>>>>> %v", string(out))
	}
}
