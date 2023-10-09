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

func TestC(t *testing.T) {
	cmd := exec.Command("/bin/sh", "-c", "cd c && make all")

	if out, err := cmd.CombinedOutput(); err != nil {
		t.Errorf("%v", string(out))
		t.Fatalf("Error executing C integration tests (%v)", err)
	}
}

func TestCPP(t *testing.T) {
	cmd := exec.Command("/bin/sh", "-c", "cd c++ && make all")

	if out, err := cmd.CombinedOutput(); err != nil {
		t.Errorf("%v", string(out))
		t.Fatalf("Error executing C++ integration tests (%v)", err)
	}
}

func TestCSharp(t *testing.T) {
	cmd := exec.Command("/bin/sh", "-c", "cd csharp && make all")

	if out, err := cmd.CombinedOutput(); err != nil {
		t.Errorf("%v", string(out))
		t.Fatalf("Error executing C# integration tests (%v)", err)
	}
}

func TestPython(t *testing.T) {
	cmd := exec.Command("/bin/sh", "-c", "cd python && make all")

	if out, err := cmd.CombinedOutput(); err != nil {
		t.Errorf("%v", string(out))
		t.Fatalf("Error executing Python integration tests (%v)", err)
	}
}

func TestCCL(t *testing.T) {
	cmd := exec.Command("/bin/sh", "-c", "cd ccl && make all")

	if out, err := cmd.CombinedOutput(); err != nil {
		t.Errorf("%v", string(out))
		t.Fatalf("Error executing CCL integration tests (%v)", err)
	}
}
