package crypto

import (
	"testing"
)

// Test checks if RandomPassword creates different passwords in 10000 runs
func TestRandomPassword(t *testing.T) {
	t.Parallel()
	passwords := []string{}
	seen := make(map[string]bool)
	result := []string{}
	for i := 1; i <= 10000; i++ {
		var randPass string
		random, _ := RandomPassword()
		randPass = random.Plaintext
		passwords = append(passwords, randPass)
		if _, ok := seen[randPass]; !ok {
			seen[randPass] = true
			result = append(result, randPass)
		}
	}

	if passwords[len(passwords)-1] != result[len(result)-1] {
		t.Errorf("last password created %q is not equal to its mapping array %q", passwords[len(passwords)-1], result[len(result)-1])
	}

	if len(result) != len(passwords) {
		t.Errorf("length of passwords created %v is not equal after duplicate search %v", len(passwords), len(result))
	}
}
